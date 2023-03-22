(cl:in-package #:asdf-user)

(defsystem #:declare-test
  :depends-on (#:declare #:declare-cst)
  :serial t
  :components
  ((:file "packages")
   (:file "test")))
