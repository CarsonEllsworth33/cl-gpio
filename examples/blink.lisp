(ql:quickload "cl-gpio")
(in-package :cl-gpio)

(defun blink () 
  (with-pins (24) ()
    (loop
      (set-pin 24 :high)
      (sleep 1)
      (set-pin 24 :low)
      (sleep 1))))

(defun blink2 () 
  (with-pins (24 25) ()
    (loop
      (set-pin 24 :high)
      (set-pin 25 :high)
      (sleep 1)
      (set-pin 24 :low)
      (set-pin 25 :low)
      (sleep 1))))

(blink)