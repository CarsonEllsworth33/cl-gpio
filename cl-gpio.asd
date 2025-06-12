(defsystem "cl-gpio"
  :version "0.2.0"
  :depends-on ("cffi")
  :pathname "src"
  :components ((:file "package")
               (:file "main")
               (:file "structs")))
