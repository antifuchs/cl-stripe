# An (experimental, toy) library for interfacing with Stripe

This library allows you to call the [stripe.com](http://stripe.com)
API from Common Lisp and should nicely present results to you.

## Installation

1. Clone this repo
2. Put it somewhere quicklisp can find it
3. To use, require it from your ASD system or `(ql:quickload :cl-stripe)` on the REPL.

## Conventions

All API functions (and data structure accessors) are exported from the
`cl-stripe` package.

The [Stripe HTTP API](https://stripe.com/api/docs) has a few conventions, and so does Common Lisp. To
get the two to talk and not cause inconvenience to you, I've
implemented a few conventions on the cl-stripe side:

* Underscores in the HTTP API (e.g., `trial_end`) become hyphens in
  the CL API (e.g., `trial-end`).
* All responses are returned as cl-stripe:sstruct objects, which are
  nicely-printing wrappers around st-json:jso (see below).
* `card` parameters can be a string to specify a token or a
  [plist](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#property_list)
  containing the fields that specify the card details.
* All API calls' function names have the form **action**-**object**. 
* A call's action is one of `retrieve`, `list`, `create`, `update`,
  `delete`, `refund`.
* A call's object is one of `charge`, `customer`, `token`,
  `subscription`, `plan`, `invoice`, `invoice-item`.
* `list` calls use the plural for objects: `customer` becomes
  `customers`. Other calls use the singular.

That's it. If you keep the
[Stripe HTTP API](https://stripe.com/api/docs) open, you should be
able to use this library.

## Data Structures

Stripe returns (not deeply) nested JSON objects. To make access to
each field easier in the absence of a nice hash syntax, cl-stripe
provides a function called `sstruct-get`. It allows you to specify
fields that define a chain through the nested object.

For example:

    (let ((customers (list-customers)))
      (sstruct-get customers :data 4 :active-card))
      
This retrieves:

1. the "data" object from the reply object,
2. then retrieves the 4th entry from that,
3. then finally retrieves the "active_card" entry from that.

`sstruct-get` field names can be specified as keywords (which will be
case-normalized to lower case, and hyphens replaced with underscores),
or as strings (which will be used verbatim).

## API Keys

Your stripe API key is a string you get from the web interface. The
default is a test key that the Stripe team provide in their
documentation. You'll probably want to set this to something else -
`(makunbound '*api-key*)` to be extra safe - especially if you're using
this in production.

You can set your API key with the `set-api-key` function, or you can
dynamically bind the `*api-key*` variable to it.

## That's it!

Have fun with this library! I hope you can make something useful with
it, and, most importantly, make some money!
