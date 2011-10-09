;;;; cl-stripe.lisp

(in-package #:cl-stripe)

;;; The basics:

(defun set-api-key (key)
  (setf *api-key* key))

;;; Resource access

(defparameter *resource-url-patterns*
  '((:charges . "/v1/charges")
    (:charge . "/v1/charges/~a")
    (:refund . "/v1/charges/~a/refund")
    (:customers . "/v1/customers")
    (:customer . "/v1/customers/~a")
    (:subscription . "/v1/customers/~a/subscription")
    (:invoices . "/v1/invoices")
    (:upcoming-invoice . "/v1/invoices/upcoming")
    (:invoice . "/v1/invoices/~a")
    (:invoice-items . "/v1/invoiceitems")
    (:invoice-item . "/v1/invoiceitems/~a")
    (:tokens . "/v1/tokens")
    (:token . "/v1/tokens/~a")
    (:plans . "/v1/plans")
    (:plan . "/v1/plans/~a")))

(defun make-resource-url (name &optional id)
  (alexandria:when-let ((path (cdr (assoc name *resource-url-patterns*))))
    (if id
        (format nil "~a~@?" *endpoint* path id)
        (concatenate 'string *endpoint* path))))

(define-condition stripe-error (error)
  ((url :initarg :url :reader stripe-error-request-url)
   (method :initarg :method :reader stripe-error-request-method)
   (request-body :initarg :body :reader stripe-error-request-body)

   (code :initarg :code :reader stripe-error-code)
   (reply :initarg :reply :reader stripe-error-reply)))

(defmethod print-object ((o stripe-error) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "while ~aing ~a: ~a"
            (stripe-error-request-method o) (stripe-error-request-url o)
            (sstruct-get (stripe-error-reply o) :error :message))))

(define-condition unknown-stripe-error (stripe-error) ())

(macrolet ((deferror (name code)
             `(progn (define-condition ,name (stripe-error) ())
                     (defmethod translate-stripe-http-code ((code (eql ,code)) reply method url body)
                       (error ',name :code code :reply reply :method method :url url :body body)))))
  
  (defgeneric translate-stripe-http-code (code structure method url body)
    (:method ((code (eql 200)) reply method url body)
      "Everything went according to plan."
      (declare (ignore method url body))
      reply)
    (:method (code reply method url body)
      "Unknown HTTP status code - something went wrong, and we don't know what."
      (error 'unknown-stripe-error :code code :reply reply :method method :url url :body body)))

  (deferror stripe-bad-request 400)
  (deferror stripe-unauthorized 401)
  (deferror stripe-request-failed 402)
  (deferror stripe-not-found 404)
  (deferror stripe-internal-error-500 500)
  (deferror stripe-internal-error-502 502)
  (deferror stripe-internal-error-503 503)
  (deferror stripe-internal-error-504 504))

(defun issue-query (resource-name &key (method :get) id parameters)
  (multiple-value-bind (response-stream code headers url)
      (drakma:http-request (make-resource-url resource-name id)
                           :method method
                           :parameters parameters
                           :basic-authorization (list *api-key* "")
                           :content-length t
                           :want-stream t)
    (declare (ignore headers))
    (let ((json-reply (jso->sstruct (st-json:read-json response-stream))))
      (translate-stripe-http-code code json-reply method url parameters))))

(let ((card-valid-keys '("number" "exp_month" "exp_year" "cvc" "name"
                         "address_line1" "address_line2" "address_zip"
                         "address_state" "address_country")))
  (defgeneric translate-request-parameter (name value)
    (:method ((name (eql :card)) (value string))
      (list (cons "card" value)))
    
    (:method ((name (eql :card)) (key-values cons))
      "Transform a PLIST into a card[] dictionary-style URL parameter list for drakma."
      (loop for (key value) on key-values by #'cddr
            for normalized-key = (find  key card-valid-keys :test #'string-equal/underscore)
            unless key
              do (cerror "Card subkey ~s is not a valid name. Expected one of ~s"
                         key card-valid-keys)
            collect (cons (format nil "card[~a]" normalized-key) value)))

    (:method ((name (eql :card)) (key-values st-json:jso))
      (loop for key in card-valid-keys
            for value = (case-insensitive-getjso key key-values)
            when value
              collect (cons (format nil "card[~a]" key) value)))

    (:method ((name (eql :card)) (key-values sstruct))
      (translate-request-parameter name (sstruct->jso key-values)))
    
    (:method ((name (eql :card)) (key-values t))
      (error "Don't know how to translate ~s to a card spec dictionary"
             key-values))

    (:method ((name symbol) (value string))
      (list (cons (substitute #\_ #\- (string-downcase (string name))) value)))

    (:method (name (value integer))
      (translate-request-parameter name (write-to-string value)))
    
    (:method ((name string) (value string))
      (list (cons name value)))))

(defun translate-request-parameters (parameters)
  "Translate PLIST parameters into an ALIST that drakma likes."
  (loop for (key value) on parameters by #'cddr
        for translated-alist = (translate-request-parameter key value)
        when translated-alist
          nconc translated-alist))

(defun issue-id-query (&rest args)
  (let ((result (apply #'issue-query args)))
    (if-let (id (sstruct-get result :id))
      (values result id)
      result)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *verb-http-methods* '((:retrieve . :get)
                                      (:list . :get)
                                      (:create . :post)
                                      (:update . :post)
                                      (:delete . :delete)
                                      (:refund . :post))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun external-symbol-p (find-symbol package)
    (do-external-symbols (sym package)
      (when (eql sym find-symbol)
        (return-from external-symbol-p t)))
    nil))

(defmacro def-api-call (verb object-and-args (&rest parameters)
                        url)
  (let* ((object (if (consp object-and-args)
                     (car object-and-args)
                     object-and-args))
         (args (when (consp object-and-args)
                 (cdr object-and-args))))
    (destructuring-bind (&key (http-resource object) id (return-id id)) args
      (let ((function-name (format-symbol :cl-stripe '#:~a-~a verb object)))
        (assert (external-symbol-p function-name *package*))
       `(defun ,function-name
            (,@(when id `(id))
             ,@(when parameters `(&rest parameters &key ,@parameters)))
          ,url
          (declare (ignore ,@parameters))
          (let ((result
                 (issue-query ,http-resource ,@(when id `(:id id))
                              :method ,(cdr (assoc verb *verb-http-methods*))
                              ,@(when parameters
                                  `(:parameters (translate-request-parameters parameters))))))
            ,(if return-id
                 `(values result (sstruct-get result :id))
                 `result)))))))

;;; The API implementation:


;;; Charges
(def-api-call :create (:charge :http-resource :charges)
  (amount currency customer card description)
  "https://stripe.com/api/docs#create_charge")

(def-api-call :retrieve (:charge :id t) ()
              "https://stripe.com/api/docs#retrieve_charge")

(def-api-call :refund (:charge :http-resource :refund :id t) (amount)
              "https://stripe.com/api/docs#refund_charge")

(def-api-call :list :charges (customer count offset)
              "https://stripe.com/api/docs#list_charges")


;;; Customers
(def-api-call :create (:customer :http-resource :customers)
  (card coupon email description plan trial-end)
  "https://stripe.com/api/docs#create_customer")

(def-api-call :retrieve (:customer :id t) ()
              "https://stripe.com/api/docs#retrieve_customer")

(def-api-call :update (:customer :id t) (card coupon email description)
              "https://stripe.com/api/docs#update_customer")

(def-api-call :delete (:customer :id t) ()
              "https://stripe.com/api/docs#delete_customer")

(def-api-call :list :customers (count offset)
              "https://stripe.com/api/docs#list_customers")


;;; Card Tokens
(def-api-call :create :token (card amount currency)
              "https://stripe.com/api/docs#create_token")

(def-api-call :retrieve (:token :id t) ()
              "https://stripe.com/api/docs#retrieve_token")



;;; Subscriptions
(def-api-call :update (:subscription :id t) (plan coupon prorate trial-end card)
              "https://stripe.com/api/docs#update_subscription")

(def-api-call :delete (:subscription :id t) (at-period-end)
              "https://stripe.com/api/docs#cancel_subscription")


;;; Plans
(def-api-call :create (:plan :http-resource :plans :return-id t)
  (id amount currency interval name trial-period-days)
  "https://stripe.com/api/docs#create_plan")

(def-api-call :retrieve (:plan :id t) ()
              "https://stripe.com/api/docs#retrieve_plan")

(def-api-call :delete (:plan :id t) ()
              "https://stripe.com/api/docs#delete_plan")

(def-api-call :list :plans (count offset)
              "https://stripe.com/api/docs#list_plans")



;;; Invoices
(def-api-call :retrieve (:invoice :id t) ()
              "https://stripe.com/api/docs#list_plans")

(def-api-call :retrieve :upcoming-invoice (customer)
              "https://stripe.com/api/docs#retrieve_customer_invoice")

(def-api-call :list :invoices (customer count offset)
              "https://stripe.com/api/docs#list_customer_invoices")


;;; Invoice items
(def-api-call :create (:invoice-item :http-resource :invoice-items)
  (customer amount currency description)
  "https://stripe.com/api/docs#create_invoiceitem")

(def-api-call :retrieve (:invoice-item :id t) ()
              "https://stripe.com/api/docs#retrieve_invoiceitem")

(def-api-call :update (:invoice-item :id t) (amount currency description)
              "https://stripe.com/api/docs#update_invoiceitem")

(def-api-call :delete (:invoice-item :id t) ()
              "https://stripe.com/api/docs#delete_invoiceitem")

(def-api-call :list :invoice-items (customer count offset)
              "https://stripe.com/api/docs#list_invoiceitems")