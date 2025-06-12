(ql:quickload "cl-gpio")

(defun blink () 
  (cl-gpio:with-pins () (24)
    (loop
      (format t "Blinking light~%")
      (cl-gpio:set-pin 24 :high)
      (sleep 1)
      (cl-gpio:set-pin 24 :low)
      (sleep 1))))

(defun button-read ()
  (cl-gpio:with-pins (23) (24)
    (loop
      (format t "Button press value: ~D~%" (cl-gpio:read-pin 23))
      (cl-gpio:set-pin 24 (cl-gpio:read-pin 23)))))