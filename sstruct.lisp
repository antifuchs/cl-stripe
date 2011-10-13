(in-package :stripe)

;;; A simple data structure for the JSON format.

;; st-json is nice, but its JSO objects print horribly and are
;; inconvenient to use if you are handling nested JSON objects. This
;; wraps them in an object such that I can use, e.g.:
;;
;;    (sstruct-get response :card :ccv)
;;
;; As an added bonus, it preserves the string keys in the json
;; response, but allows lookups that are case-insensitive and -/_
;; agnostic..



;;; jso functions,
;; completely cribbed from st-json, but using our own,
;; underscore-agnostic string comparison instead of string=:
(progn
  (defun char-equal/underscore (c1 c2)
    (char-equal (if (member c1 '(#\- #\_))
                    #\-
                    c1)
                (if (member c2 '(#\- #\_))
                    #\-
                    c2)))

  (defun string-equal/underscore (s1 s2)
    (setf s1 (string s1)
          s2 (string s2))
    (and (= (length s1) (length s2))
         (every #'char-equal/underscore s1 s2)))

  (defun case-insensitive-getjso (key jso)
    "Fetch a value from a JS object. Returns a second value like
gethash."
    (let ((pair (assoc key (st-json::jso-alist jso) :test #'string-equal/underscore)))
      (values (cdr pair) (and pair t))))
  
  (defun (setf case-insensitive-getjso) (val key map)
    "Store a value in a JS object."
    (let ((pair (assoc key (st-json::jso-alist map) :test #'string-equal/underscore)))
      (if pair
          (setf (cdr pair) val)
          (prog1 val (push (cons key val) (st-json::jso-alist map)))))))

(defun jso-p (o)
  (typep o 'st-json:jso))

;;; The sstruct data structure and its interface:

(defclass sstruct ()
  ((jso :initarg :jso :accessor sstruct-jso))
  (:documentation "A structure for JSON communication with stripe.

Stripe communicates via nested JSON objects. This structure exists to
allow easier access to the response and request fields. Field access
goes through `sstruct-get`. An sstruct can be converted to plists and
st-json:jso objects via the `sstruct->jso` function."))

(defvar *print-sstruct-unreadably* t)

(defun recursively-treat-potential-jso (potential-jso if-jso if-not-jso)
  (cond ((jso-p potential-jso)
         (funcall if-jso potential-jso))
        ((consp potential-jso)
         (mapcar (lambda (potential-jso)
                   (recursively-treat-potential-jso potential-jso if-jso if-not-jso))
                 potential-jso))
        (t (funcall if-not-jso potential-jso))))

(defmethod print-object ((o sstruct) stream)
  (labels ((print-jso-nicely (jso)
             (write-string "{ " stream)
             (let ((firstp t))
               (st-json:mapjso (lambda (key val)
                                 (if firstp
                                     (setf firstp nil)
                                     (write-string ", " stream))
                                 (write-string key stream)
                                 (write-string ": " stream)
                                 (recursively-treat-potential-jso val
                                                                  #'print-jso-nicely
                                                                  (lambda (val)
                                                                    (write val :stream stream))))
                               jso))
             (write-string " }" stream)))
    (if *print-sstruct-unreadably*
        (print-unreadable-object (o stream :type t)
          (let ((*print-sstruct-unreadably* nil))
            (print-jso-nicely (sstruct-jso o))))
        (print-jso-nicely (sstruct-jso o)))))

(defun jso->sstruct (jso)
  (make-instance 'sstruct :jso jso))

(defun sstruct->jso (sstruct)
  (sstruct-jso sstruct))

(defun sstruct-get (sstruct &rest keys)
  "Case- and dash-insensitively get keys from nested JSON objects."
  (let ((current-object (sstruct-jso sstruct)))
    (dolist (search-key keys)
      (setf current-object
            (cond ((numberp search-key) (elt current-object search-key))
                  (t (case-insensitive-getjso search-key current-object)))))
    (recursively-treat-potential-jso current-object #'jso->sstruct #'identity)))

(defun (setf sstruct-get) (new-val sstruct &rest keys)
  (let ((innermost-struct (apply #'sstruct-get sstruct (butlast keys))))
    (setf (case-insensitive-getjso (car (last keys))
                                   (sstruct->jso innermost-struct))
          new-val)))
