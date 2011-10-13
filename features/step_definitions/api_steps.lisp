(When* #?{^I use the API key "([^"]*)"$} (key)
       (stripe:set-default-api-key key))
