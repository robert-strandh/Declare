(cl:in-package #:common-lisp-user)

(defpackage #:declare-test
  (:use #:common-lisp)
  (:local-nicknames (#:d #:declare))
  (:export #:test))
