(cl:in-package #:declare)

;;;; Let us start by establishing some terminology.  Every declaration
;;;; specifier, including implementation-specific declaration
;;;; specifiers, starts with a declaration identifier which is followed
;;;; by zero or more arguments.  Every standard declaration specifier
;;;; starts with a declaration identifier and ends with a (possibly
;;;; empty) sequence of what we will call "targets".  A target can have
;;;; several different shapes:
;;;;
;;;;   * It can be a symbol.  Then it is either:
;;;;
;;;;       - the name of a variable, for example in the IGNORE
;;;;         declaration specifier,
;;;;
;;;;       - the name of a function, for example in the INLINE
;;;;         declaration specifier,
;;;;
;;;;       - a quality, in the case of the OPTIMIZE declaration
;;;;         specifier.
;;;;
;;;;   * It can be a list of the form (FUNCTION symbol), for example
;;;;     in the IGNORE declaration specifier when a function is meant
;;;;     to be ignored.
;;;;
;;;;   * It can be a list of the form (quality value) as with the
;;;;     OPTIMIZE declaration specifier.
;;;;
;;;; Most standard declaration specifiers consist of only a declaration
;;;; identifier followed by a sequence of targets.  Only the TYPE and
;;;; the FTYPE declaration specifiers contain an additional element (a
;;;; type specifier) between the declaration identifier and the
;;;; targets.
;;;;
;;;; Implementations are allowed to add nonstandard declaration
;;;; specifiers.  It is up to the implementation to interpret the
;;;; arguments of such declaration specifiers.  They may not have the
;;;; same structure as the standard declaration specifiers.
;;;;
;;;; A standard declaration specifier is said to be "canonical" if it
;;;; has exactly one target.  An essential part of this code takes a
;;;; standard declaration specifier with zero or more targets and
;;;; turns it into a list of canonical declaration specifiers.  This
;;;; library can not make any assumptions about how to canonicalize
;;;; nonstandard declaration specifiers.  Client code can define
;;;; methods on the generic function
;;;; CANONICALIZE-DECLARATION-SPECIFIER to canonicalize the
;;;; declaration specifiers specific to that client.
;;;;
;;;; There is an additional complication that must be dealt with in
;;;; code to canonicalize declaration specifiers.  Application code
;;;; may contain nonstandard declaration specifiers that are specific
;;;; to an implementation other than the one used by client code.
;;;; Neither the library nor the client of the library can make
;;;; assumptions about the arguments of such declaration specifiers.
;;;; But such declaration specifiers must be flagged as such so that
;;;; they can be ignored by the library.  Recall that a declaration
;;;; specifier may start with a type specifier as an abbreviation for
;;;; the TYPE declaration specifier.  If nonstandard declaration
;;;; specifiers are not flagged as such, the library assumes it is an
;;;; abbreviated TYPE declaration specifier.  We deal with this issue
;;;; by taking an additional argument containing a list of declaration
;;;; specifiers to ignore.  The result of canonicalization does not
;;;; contain such declaration specifiers.  A declaration specifier
;;;; that is an abbreviation for a type declaration is canonicalized
;;;; as an explicit TYPE declaration specifier.

;;; This default method is called only when the second argument is
;;; known to be a type specifier.  Declaration identifiers that should
;;; be ignored have been eliminated when this function is called, and
;;; client-specific declaration identifiers must be handled by
;;; client-specific methods on this generic functions.  So we are left
;;; with only type specifiers.
(defmethod canonicalize-declaration-specifier
    (client type-specifier declaration-specifier)
  (canonicalize-declaration-specifier
   client 'type (cons (cook 'type) declaration-specifier)))

;;; Given a PREFIX P and a list of ITEMS, say (I1 I2 ... In), return a
;;; list of the items prefixed with P, i.e. ((P I1) (P I2) ... (P
;;; In)).  The twist is that the list of items is represented in the
;;; form of a concrete syntax tree.
(defun map-prefix (prefix items)
  (loop for remaining = items then (rest remaining)
        until (null remaining)
        collect (cons prefix (cons (first remaining) nil))))

(progn
  .
  #.(loop for declaration-identifier in
          '(declaration dynamic-extent ignore ignorable
            inline notinline optimize special)
          collect `(defmethod canonicalize-declaration-specifier
                       (client
                        (declaration-identifier (eql ',declaration-identifier))
                        declaration-specifier)
                     (map-prefix (first declaration-specifier)
                                 (rest  declaration-specifier)))))

(defun canonicalize-type-and-ftype (declaration-specifier)
  (loop with declaration-identifier = (first declaration-specifier)
        with type-specifier = (first (rest declaration-specifier))
        with targets = (rest (rest declaration-specifier))
        for remaining = targets then (rest remaining)
        until (null remaining)
        collect (cons declaration-identifier
                      (cons type-specifier
                            (cons (first remaining)
                                  nil)))))

(defmethod canonicalize-declaration-specifier
    (client (declaration-identifier (eql 'type)) declaration-specifier)
  (canonicalize-type-and-ftype declaration-specifier))

(defmethod canonicalize-declaration-specifier
    (client (declaration-identifier (eql 'ftype)) declaration-specifier)
  (canonicalize-type-and-ftype declaration-specifier))

(defun canonicalize-declaration-specifiers
    (client declaration-specifiers)
  (loop with *client* = client
        with ignored-declaration-identifiers
          = (ignored-declaration-identifiers client)
        for remaining = declaration-specifiers then (rest remaining)
        until (null remaining)
        append
        (let* ((declaration-specifier (first remaining))
               (declaration-identifier (raw (first declaration-specifier))))
          (if (member declaration-identifier ignored-declaration-identifiers)
              '()
              (canonicalize-declaration-specifier
               client declaration-identifier declaration-specifier)))))

; LocalWords:  canonicalize canonicalization canonicalized
