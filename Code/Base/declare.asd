(cl:in-package #:asdf-user)

(defsystem #:declare
  :serial t
  :components
  ((:file "packages")
   (:file "canonicalize")))
