(defsystem "cl-gpio"
  :version "0.1.0"
  :depends-on ("cffi")
  :pathname "src"
  :components ((:file "package")
               (:file "main")
               (:file "structs")))