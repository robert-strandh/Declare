(cl:in-package #:asdf-user)

(defsystem #:declare-cst
  :depends-on (#:declare
               #:concrete-syntax-tree)
  :serial t
  :components
  ((:file "client")))

  
               
