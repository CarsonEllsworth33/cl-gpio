(defcstruct gpiod-chip
  (fd :int)
  (path (:pointer :char))) 

(defcstruct gpiod-chip-info
  (num-lines :size)
  (name :char :count 33)
  (label :char :count 33)) 

(defcenum :gpiod-line-direction
  (gpiod-line-direction-as-is 1)
  gpiod-line-direction-input
  gpiod-line-direction-output) 

(defcenum :gpiod-line-value
  (gpiod-line-value-error -1)
  (gpiod-line-value-inactive 0)
  (gpiod-line-value-active 1)) 

(defcenum :gpiod-line-bias
  (gpiod-line-bias-as-is 1)
  gpiod-line-bias-unknown
  gpiod-line-bias-disabled
  gpiod-line-bias-pull-up
  gpiod-line-bias-pull-down) 

(defcenum :gpiod-line-edge
  (gpiod-line-edge-none 1)
  gpiod-line-edge-rising
  gpiod-line-edge-falling
  gpiod-line-edge-both) 

(defcenum :gpiod-line-clock
  (gpiod-line-clock-monotonic 1)
  gpiod-line-clock-realtime
  gpiod-line-clock-hte) 

(defcstruct gpiod-line-info
  (offset :uint)
  (name :char :count 33)
  (used :bool)
  (consumer :char :count 33)
  (direction :gpiod-line-direction)
  (active-low :bool)
  (bias :gpiod-line-bias)
  (edge :gpiod-line-edge)
  (event-clock :gpiod-line-clock)
  (debounced :bool)
  (debounce-period-us :ulong))

(defcstruct gpiod-line-request
  (chip-name (:pointer :char))
  (offset :uint :count 64)
  (num-lines :size)
  (fd :int)) 

