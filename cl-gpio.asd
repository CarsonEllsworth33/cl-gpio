(defsystem "cl-gpio"
  :version "0.0.1"
  :depends-on ("cffi")
  :pathname "src"
  :components ((:file "main")
               (:file "structs")))