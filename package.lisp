;;;; package.lisp

(defpackage #:cl-stripe
  (:use #:cl #:alexandria)
  (:export #:sstruct-get
           #:sstruct->jso
           #:jso->sstruct
           
           #:set-api-key
           #:*api-key*
           
           #:create-charge
           #:retrieve-charge
           #:refund-charge
           #:list-charges

           #:create-customer
           #:retrieve-customer
           #:update-customer
           #:delete-customer
           #:list-customers

           #:create-token
           #:retrieve-token

           #:update-subscription
           #:delete-subscription

           #:create-plan
           #:retrieve-plan
           #:delete-plan
           #:list-plans

           #:retrieve-invoice
           #:retrieve-upcoming-invoice
           #:list-invoices

           #:create-invoice-item
           #:retrieve-invoice-item
           #:update-invoice-item
           #:delete-invoice-item
           #:list-invoice-items))

