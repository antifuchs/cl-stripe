;; -*- lisp -*-

(defun lookup-currency-symbol (currency-symbol)
  (values
   (cdr (assoc currency-symbol '(("$" . "usd")
                                 ("ct" . "usd"))
               :test 'string-equal))
   (if (string-equal currency-symbol "$")
       100
       1)))

;; TODO: generate the card number randomly
(defvar *random-card* (list :number "4242424242424242" :exp_month "12" :exp_year "2020" :cvc "123"
                     :name "Tester Oberloisl"
                     :address-line1 "555 Random Avenue" :address-line2 "Apt 9"
                     :address-zip "55959" :address-state "Ohai-o"
                     :address_country "USA"))

(Then* #?{^the charge should exist$} ()
       (assert (nth-value 1 (stripe:retrieve-charge (var :charge-id)))))

(Then* #?{^the charge should be for ([^0-9]+)\s*([0-9]+)$} (currency-symbol amount)
       (let ((amount (parse-integer amount)))
         (multiple-value-bind (currency multiplier) (lookup-currency-symbol currency-symbol)
           (assert (string-equal (stripe:sstruct-get (var :charge) :currency) currency) ()
                   "Currencies are not equal: ~s != ~s" (stripe:sstruct-get :currency) currency)
           (assert (= (stripe:sstruct-get (var :charge) :amount) (* amount multiplier)) ()
                   "Amounts are not equal: ~s != ~s"
                   (/ (stripe:sstruct-get (var :charge) :amount) multiplier) amount))))

(When* #?{^I charge ([^0-9]+)\s*([0-9]+) to (a random credit card|the token)$} (currency-symbol amount what)
       (if (string= what "a random credit card")
           (setf (var :card) *random-card*)
           (setf (var :card) (var :token-id)))
       (multiple-value-bind (currency multiplier) (lookup-currency-symbol currency-symbol)
         (setf (var :currency) currency)
         (setf (var :amount) (parse-integer amount :junk-allowed nil))
         (setf (var :amount) (* multiplier (var :amount))))
       
       (multiple-value-bind (charge id)
           (stripe:create-charge :amount (var :amount)
                                 :currency (var :currency)
                                 :description (format nil "Cucumber test charge ~a" (get-universal-time))
                                 :card (var :card))
         (assert (stringp id) (id)
                 "ID is not a string: ~s" id)
         (setf (var :charge) charge
               (var :charge-id) id)))

