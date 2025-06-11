;(load "~/dunderscore/workspaces/git-repos/cl-gpio/main.lisp")
(in-package :cl-gpio) 

(with-pins '(5 26) nil
  (set-pin 26 :high)
  (sleep 3)
  (set-pin 26 :low)
  (sleep 7)
  (set-pin 5 :active)
  (sleep 3)
  (set-pin 5 :inactive))
