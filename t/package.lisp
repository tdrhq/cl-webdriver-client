(defpackage webdriver-client-test
  (:use :cl :webdriver-client :webdriver-client-utils :prove))

(in-package :webdriver-client-test)

;; slime detection
(setf prove:*enable-colors* (interactive-stream-p *standard-input*))

(defparameter *headless* '(("goog:chromeOptions" . ((:args . #("--headless"))))))
(setf *timeout* 5)
