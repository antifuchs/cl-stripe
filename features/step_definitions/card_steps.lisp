(When* #?{^I create a token for a random credit card$} ()
       (setf (var :card) *random-card*
             (var :amount) nil
             (var :currency) nil)
       (multiple-value-bind (token id)
           (stripe:create-token :card (var :card))
         (assert (stringp id) (id)
                 "ID is not a string: ~s" id)
         (setf (var :token) token
               (var :token-id) id)))
