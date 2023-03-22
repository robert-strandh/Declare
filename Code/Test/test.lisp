(cl:in-package #:declare-test)

(defparameter *ignored-declaration-identifiers*
  '(foo bar baz))

(defclass s-expression-client (d:s-expression-client)
  ())

(defmethod d:ignored-declaration-identifiers
    ((client s-expression-client))
  *ignored-declaration-identifiers*)

(defun test1 (client)
  (assert (equal (d:canonicalize-declaration-specifiers
                  client
                  '((ignore a b c)
                    (ignorable d e f)
                    (dynamic-extent g h)
                    (foo hello j k l)
                    (ignorable)
                    (bar 1 2 3)))
                 '((ignore a) (ignore b) (ignore c)
                   (ignorable d) (ignorable e) (ignorable f)
                   (dynamic-extent g) (dynamic-extent h)))))

(defun test ()
  (let ((client (make-instance 's-expression-client)))
    (test1 client)))
