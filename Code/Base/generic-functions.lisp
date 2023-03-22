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

;;; This generic function takes three arguments.  The first argument
;;; is a CLIENT object.  The second argument is a declaration
;;; identifier (as an ordinary Common Lisp symbol), or perhaps a type
;;; specifier (see below).  The third argument is the entire
;;; declaration specifier to canonicalize.  As hinted above, typically
;;; the second argument is a declaration identifier.  But because a
;;; declaration specifier where the first element is a type specifier
;;; can be used as an abbreviation for the TYPE declaration specifier,
;;; the DECLARATION-IDENTIFIER argument can also be an arbitrary type
;;; specifier.  We handle that case by defining a default method on
;;; this generic function.  That default method is called only when
;;; the DECLARATION-IDENTIFIER argument is known to be a type
;;; specifier.
(defgeneric canonicalize-declaration-specifier
    (client declaration-identifier declaration-specifier))
