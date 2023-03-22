(cl:in-package #:declare)

;;;; This library takes expressions apart using only the four
;;;; functions FIRST, REST, NULL, and ATOM (and perhaps higher-level
;;;; functions built upon these four), and it constructs expressions
;;;; using only the function CONS (and again, perhaps higher-level
;;;; functions built upon this function).  These five functions are
;;;; not the standard Common Lisp functions with those names, and they
;;;; are instead defined by the library.  By doing it this way, we can
;;;; allow for representations of expressions other than
;;;; S-expressions, and in particular, we support Concrete Syntax
;;;; Trees (CSTs).  The way we do this is to have those functions
;;;; trampoline to functions defined in a CLIENT object.  Client code
;;;; can then decide on the concrete implementation of those functions
;;;; without impacting the library code itself.  In addition

;;; This client class can be used as a superclass of a client-specific
;;; client class.  It uses the standard Common Lisp functions FIRST,
;;; REST, NULL, and ATOM to take apart expressions.
(defclass s-expression-client ()
  ((%first-function
    :initform #'cl:first
    :reader first-function)
   (%rest-function
    :initform #'cl:rest
    :reader rest-function)
   (%null-function
    :initform #'cl:null
    :reader null-function)
   (%atom-function
    :initform #'cl:atom
    :reader atom-function)
   (%cons-function
    :initform #'cl:cons
    :reader cons-function)
   (%raw-function
    :initform #'cl:identity
    :reader raw-function)))

(defvar *client*)

(defun first (expression)
  (funcall (first-function *client*) expression))

(defun rest (expression)
  (funcall (rest-function *client*) expression))

(defun null (expression)
  (funcall (null-function *client*) expression))

(defun atom (expression)
  (funcall (atom-function *client*) expression))

(defun cons (expression1 expression2)
  (funcall (cons-function *client*) expression1 expression2))

(defun raw (expression)
  (funcall (raw-function *client*) expression))
