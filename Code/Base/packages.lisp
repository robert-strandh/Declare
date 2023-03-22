(cl:in-package #:common-lisp-user)

(defpackage #:declare
  (:use #:common-lisp)
  (:shadow #:first #:rest #:null #:atom #:cons)
  (:export
   #:canonicalize-declaration-specifier
   #:canonicalize-declaration))
