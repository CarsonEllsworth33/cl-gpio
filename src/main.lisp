(in-package :cl-gpio)
(defvar *chip-path* "/dev/gpiochip0")
(defparameter *output-pins* nil)
(defparameter *input-pins* nil)

(defmacro with-pins (input-pins output-pins &body body)
  `(let ((chip (gpiod-chip-open *chip-path*))
         (line-cfg (gpiod-line-config-new))
         (output-pin-settings (gpiod-line-settings-new))
         (output-offset (cffi:foreign-alloc :uint :initial-contents ',output-pins))
         (input-pin-settings (gpiod-line-settings-new))
         (input-offset (cffi:foreign-alloc :uint :initial-contents ',input-pins)))
     (setf *output-pins* ',output-pins)
     (setf *input-pins* ',input-pins)
     (when ',output-pins
       (gpiod-line-settings-set-direction output-pin-settings gpiod-line-direction-output)
       (gpiod-line-settings-set-output-value output-pin-settings gpiod-line-value-inactive)
       (gpiod-line-config-add-line-settings line-cfg output-offset (length ',output-pins) output-pin-settings))
     (when ',input-pins
       (gpiod-line-settings-set-direction input-pin-settings gpiod-line-direction-input)
       (gpiod-line-config-add-line-settings line-cfg input-offset (length ',input-pins) input-pin-settings))
     (let ((request (gpiod-chip-request-lines chip (cffi:null-pointer) line-cfg)))
       ;; User forms injected here for pin manipulation
       ,@body)
     (setf *output-pins* nil)
     (setf *input-pins* nil)
     (gpiod-chip-close chip)
     (cffi:foreign-free output-offset)
     (gpiod-line-settings-free output-pin-settings)
     (cffi:foreign-free input-offset)
     (gpiod-line-settings-free input-pin-settings)
     (gpiod-line-config-free line-cfg)))

(defmacro set-pin (pin output-value)
  "Set a specific output PIN High or Low.
This is meant to be used within the WITH-PINS macro and only one PIN is to be set from the list of specified pins in WITH-PINS per call to SET-PIN.
Valid OUTPUT-VALUE values are 
:high :active for logic high
:low :inactive for logic low"
  (cond ((or (eql output-value :active)
              (eql output-value :high))
         (setf output-value 'gpiod-line-value-active))
        ((or (eql output-value :inactive)
             (eql output-value :low))
         (setf output-value 'gpiod-line-value-inactive)))
  `(if (member ,pin *output-pins*)
       (gpiod-line-request-set-value request ,pin ,output-value)))

(defmacro read-pin (pin)
  `(if (member ,pin *input-pins*)
       (gpiod-line-request-get-value request ,pin)))


