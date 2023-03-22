(cl:in-package #:declare)

;;;; This library needs to construct and take apart expressions.  But
;;;; we do not want to commit to a particular representation of those
;;;; expressions.  In particular, we want to support both
;;;; s-expressions and concrete syntax trees as possible
;;;; representations.  For that reason, we want to abstract out the
;;;; exact functions and objects used for this purpose.  To take
;;;; expressions apart, we use only the four functions FIRST, REST,
;;;; NULL, and ATOM (and perhaps higher-level functions built upon
;;;; these four).  To construct expressions, we use only the function
;;;; CONS and the object NIL.  All these names are specific to this
;;;; library, and unrelated to the Common Lisp symbols with the same
;;;; name.  We accomplish the desired behavior by having our functions
;;;; trampoline to functions defined in the CLIENT object, and we
;;;; define NIL as a symbol macro that accesses a client-specific
;;;; object in the CLIENT object.  We also define a function RAW that,
;;;; given some object in the chosen representation of expressions,
;;;; returns the underlying Common Lisp object being represented.

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
   (%nil-object
    :initform cl:nil
    :reader nil-object)
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

(define-symbol-macro nil (nil-object *client*))

(defun raw (expression)
  (funcall (raw-function *client*) expression))
