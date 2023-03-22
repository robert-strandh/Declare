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

;;; This generic function takes a CLIENT object and the two parts of a
;;; declaration specifier, typically the declaration identifier (a
;;; symbol) and the arguments.  But because a declaration specifier
;;; where the first element is a type specifier can be used as an
;;; abbreviation for the TYPE declaration specifier, the
;;; DECLARATION-IDENTIFIER argument can also be an arbitrary type
;;; specifier.  We handle that case by defining a default method on
;;; this generic function.  That default method is called only when
;;; the DECLARATION-IDENTIFIER argument is known to be a type
;;; specifier.
(defgeneric canonicalize-declaration-specifier
    (client declaration-identifier arguments))
