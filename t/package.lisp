(defpackage cl-selenium-test
  (:use :cl :cl-selenium :cl-selenium-utils :prove))

(in-package :cl-selenium-test)

;; slime detection
(setf prove:*enable-colors* (interactive-stream-p *standard-input*))

(defparameter *headless* '(("chrome:browserOptions" . ((:args . #("--headless"))))))
(setf *timeout* 5)
