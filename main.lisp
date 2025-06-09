(asdf:load-system :cffi)

(defpackage :cl-gpio
  (:use :cl :cffi))

(in-package :cl-gpio)

(defconstant GPIO-MAX-NAME-SIZE 32) 
(defconstant GPIO-V2-LINE-MAX 64) 

(define-foreign-library libgpiod
  (t (:default "libgpiod"))) 

(use-foreign-library libgpiod) 

(defmacro array-of-size (size)
  "defcstruct doesn't allow forms passed into :count they have to be of type REAL"
  (let ((calc-size (eval size)))
    `,calc-size)) 

(defcstruct gpiod-chip
  (fd :int)
  (path (:pointer :char))) 

(defcstruct gpiod-chip-info
  (num-lines :size)
  (name :char :count #.(1+ GPIO-V2-LINE-MAX))
  (label :char :count #.(1+ GPIO-V2-LINE-MAX))) 

(defcenum gpiod-line-direction
  (gpiod-line-direction-as-is 1)
  gpiod-line-direction-input
  gpiod-line-direction-output) 

(defcenum gpiod-line-value
  (gpiod-line-value-error -1)
  (gpiod-line-value-inactive 0)
  (gpiod-line-value-active 1)) 

(defcenum gpiod-line-bias
  (gpiod-line-bias-as-is 1)
  gpiod-line-bias-unknown
  gpiod-line-bias-disabled
  gpiod-line-bias-pull-up
  gpiod-line-bias-pull-down) 

(defcenum gpiod-line-edge
  (gpiod-line-edge-none 1)
  gpiod-line-edge-rising
  gpiod-line-edge-falling
  gpiod-line-edge-both) 

(defcenum gpiod-line-clock
  (gpiod-line-clock-monotonic 1)
  gpiod-line-clock-realtime
  gpiod-line-clock-hte) 

(defcenum gpiod-line-drive
  (gpiod-line-drive-push-pull 1)
  gpiod-line-drive-open-drain
  gpiod-line-drive-open-source)

(defcstruct gpiod-line-info
  (offset :uint)
  (name :char :count #.(1+ GPIO-V2-LINE-MAX))
  (used :bool)
  (consumer :char :count #.(1+ GPIO-V2-LINE-MAX))
  (direction gpiod-line-direction)
  (active-low :bool)
  (bias gpiod-line-bias)
  (edge gpiod-line-edge)
  (event-clock gpiod-line-clock)
  (debounced :bool)
  (debounce-period-us :ulong))

(defcstruct gpiod-line-request
  (chip-name (:pointer :char))
  (offset :uint :count #.GPIO-V2-LINE-MAX) ; GPIO-V2-LINES-MAX
  (num-lines :size)
  (fd :int)) 

(defcstruct gpiod-line-settings
  (direction gpiod-line-direction)
  (edge-detection gpiod-line-edge)
  (drive gpiod-line-drive)
  (bias gpiod-line-bias)
  (active-low :bool)
  (event-clock gpiod-line-clock)
  (debounce-period-us :long)
  (output-value gpiod-line-value))

(defcstruct settings-node
  (prev (:pointer (:struct settings-node)))
  (next (:pointer (:struct settings-node)))
  (settings (:pointer (:struct gpiod-line-settings)))
  (refcnt :uint))

(defcstruct per-line-config
  (offset :uint)
  (node (:pointer (:struct settings-node))))

(defcstruct gpiod-line-config
  (line-configs (:struct per-line-config) :count #.GPIO-V2-LINE-MAX)
  (num-configs :size)
  (output-values gpiod-line-value :count #.GPIO-V2-LINE-MAX)
  (num-output-values :size)
  (sref-list (:pointer (:struct settings-node))))

(defcstruct gpiod-request-config
  (consumer :char :count #.GPIO-MAX-NAME-SIZE)
  (event_buffer_size :size))


(defcfun "gpiod_chip_open" (:pointer (:struct gpiod-chip))
  "Open a chip by path.
The GPIO chip returned must be closed with gpio-chip-close"
  (path :string)) 
(defcfun "gpiod_chip_close" :void
  "close chip and release resources"
  (chip (:pointer (:struct gpiod-chip))))
(defcfun "gpiod_chip_get_line_info" (:pointer (:struct gpiod-line-info))
  "Get a snapshot of information about a line"
  (chip (:pointer (:struct gpiod-chip)))
  (offset :uint))
(defcfun "gpiod_chip_request_lines" (:pointer (:struct gpiod-line-request))
  (chip (:pointer (:struct gpiod-chip)))
  (req-cfg (:pointer (:struct gpiod-request-config)))
  (line-cfg (:pointer (:struct gpiod-line-config))))

(defcfun "gpiod_line_info_free" :void
  (info (:pointer (:struct gpiod-line-info)))) 
(defcfun "gpiod_line_info_get_direction" gpiod-line-direction
  (info (:pointer (:struct gpiod-line-info)))) 


(defcfun "gpiod_line_settings_new" (:pointer (:struct gpiod-line-settings)))
(defcfun "gpiod_line_settings_free" :void
  (settings (:pointer (:struct gpiod-line-settings))))
(defcfun "gpiod_line_settings_set_direction" :int
  "Set direction
0 on success and -1 on error"
  (settings (:pointer (:struct gpiod-line-settings)))
  (direction gpiod-line-direction))
(defcfun "gpiod_line_settings_get_direction" gpiod-line-direction
  (settings (:pointer (:struct gpiod-line-settings))))
(defcfun "gpiod_line_settings_set_output_value" :int
  (settings (:pointer (:struct gpiod-line-settings)))
  (value gpiod-line-value))

(defcfun "gpiod_line_config_new" (:pointer (:struct gpiod-line-config)))
(defcfun "gpiod_line_config_free" :void
  (config (:pointer (:struct gpiod-line-config))))
(defcfun "gpiod_line_config_add_line_settings" :int
  (config (:pointer (:struct gpiod-line-config)))
  (offsets (:pointer :uint))
  (num_offsets :size)
  (settings (:pointer (:struct gpiod-line-settings))))

(defcfun "gpiod_line_request_release" :void
  (request (:pointer (:struct gpiod-line-request))))
(defcfun "gpiod_line_request_set_value" :int
  (request (:pointer (:struct gpiod-line-request)))
  (offset :uint)
  (value gpiod-line-value))

(defcfun "gpiod_request_config_new" (:pointer (:struct gpiod-request-config)))
(defcfun "gpiod_request_config_free" :void
  (config (:pointer (:struct gpiod-request-config))))
(defcfun "gpiod_request_config_set_consumer" :void
  (config (:pointer (:struct gpiod-request-config)))
  (consumer :string))
(defcfun "gpiod_request_config_set_consumer" :string
  (config (:pointer (:struct gpiod-request-config))))

(defmacro with-chip (path &body body) )