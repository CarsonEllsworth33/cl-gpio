(in-package :cl-gpio)

(defmacro with-pins (pins (&key (path "/dev/gpiochip0") (direction :output)) &body body)
  (if (string= (symbol-name direction) "OUTPUT")
      (setf direction gpiod-line-direction-output)
      (setf direction gpiod-line-direction-input))
  `(progn
     (let ((chip (gpiod-chip-open ,path))
           (settings (gpiod-line-settings-new))
           (line-cfg (gpiod-line-config-new))
           (offset (cffi:foreign-alloc :uint :initial-contents ',pins))
           (request ()))
       (gpiod-line-settings-set-direction settings ,direction)
       (gpiod-line-settings-set-output-value settings gpiod-line-value-inactive)
       (gpiod-line-config-add-line-settings line-cfg offset (length ',pins) settings)
       (setf request (gpiod-chip-request-lines chip (cffi:null-pointer) line-cfg))
       ,@body
       (gpiod-chip-close chip)
       (cffi:foreign-free offset)
       (gpiod-line-settings-free settings)
       (gpiod-line-config-free line-cfg))))

(defmacro set-pin (pin output-value)
  "Set a specific PIN High or Low.
This is meant to be used within the WITH-PINS macro and only one PIN is to be set from the list of specified pins in WITH-PINS per call to SET-PIN.
Valid OUTPUT-VALUE values are 
:high :active for logic high
:low :inactive for logic low"
  (cond ((or (string= (symbol-name output-value) "ACTIVE")
              (string= (symbol-name output-value) "HIGH"))
         (setf output-value 'gpiod-line-value-active))
        ((or (string= (symbol-name output-value) "INACTIVE")
             (string= (symbol-name output-value) "LOW"))
         (setf output-value 'gpiod-line-value-inactive))
        (t (setf output-value 'gpiod-line-value-inactive)))
  `(gpiod-line-request-set-value request ,pin ,output-value))



