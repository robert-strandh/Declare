(cl:in-package #:declare)

;;; This client class can be used as a superclass of a client-specific
;;; client class.  It uses the functions FIRST, REST, NULL, and ATOM
;;; compatible with the CST library to take apart expressions.
(defclass cst-client ()
  ((%first-function
    :initform #'cst:first
    :reader first-function)
   (%rest-function
    :initform #'cst:rest
    :reader rest-function)
   (%null-function
    :initform #'cst:null
    :reader null-function)
   (%atom-function
    :initform #'cst:atom
    :reader atom-function)
   (%cons-function
    :initform #'cst:cons
    :reader cons-function)
   (%nil-object
    :initform (cst:cst-from-expression cl:nil)
    :reader nil-object)
   (%raw-function
    :initform #'cst:raw
    :reader raw-function)
   (%cook-function
    :initform (lambda (expression) (cst:cst-from-expression expression))
    :reader cook-function)))

(export 'cst-client)
