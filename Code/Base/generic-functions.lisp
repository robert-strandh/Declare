(cl:in-package #:declare)


;;; This generic function is called by the library in order to
;;; determine which declaration specifiers in the input that should
;;; not be part of the output.  This function returns a list of
;;; symbols.  Any declaration specifier in the input with a
;;; declaration identifier that is a member of this list is excluded
;;; from the result.

(defgeneric ignored-declaration-identifiers (client))

;;; By default, we return the empty list.
(defmethod ignored-declaration-identifiers (client)
  '())
