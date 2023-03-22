(cl:in-package #:common-lisp-user)

(defpackage #:declare
  (:use #:common-lisp)
  (:export
   #:canonicalize-declaration-specifier
   #:canonicalize-declaration))
