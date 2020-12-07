(asdf:defsystem :isthmus
  :name "isthmus"
  :depends-on (:closer-mop :defclass-std)
  :serial t
  :components
  ((:file "package")
   (:file "main")))
