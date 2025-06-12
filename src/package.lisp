(defpackage :cl-gpio
  (:use :cl :cffi)
  (:export 
   #:with-pins
   #:set-pin
   #:read-pin
   #:*chip-path*))
