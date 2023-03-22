(cl:in-package #:common-lisp-user)

(defpackage #:declare
  (:use #:common-lisp)
  (:shadow #:first #:rest #:null #:atom #:cons #:nil)
  (:export
   #:canonicalize-declaration-specifier
   #:canonicalize-declaration-specifiers
   #:s-expression-client
   #:ignored-declaration-identifiers))
