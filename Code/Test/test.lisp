(cl:in-package #:declare-test)

(defparameter *ignored-declaration-identifiers*
  '(foo bar baz))

(defclass s-expression-client (d:s-expression-client)
  ())
