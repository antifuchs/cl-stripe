;;;; cl-stripe.asd

(asdf:defsystem #:cl-stripe
  :serial t
  :depends-on (#:st-json
               #:drakma
               #:alexandria)
  :components ((:file "package")
               (:file "variables")
               (:file "sstruct")
               (:file "cl-stripe")))

