(defpackage webdriver-test
  (:use :cl :webdriver :webdriver-utils :prove))

(in-package :webdriver-test)

;; slime detection
(setf prove:*enable-colors* (interactive-stream-p *standard-input*))

(defparameter *headless* '(("goog:chromeOptions" . ((:args . #("--headless"))))))
(setf *timeout* 5)
