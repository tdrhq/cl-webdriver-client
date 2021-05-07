(defpackage cl-selenium-test
  (:use :cl :cl-selenium :cl-selenium-utils :prove))

(in-package :cl-selenium-test)

;; slime detection
(setf prove:*enable-colors*
      (string/= (package-name (symbol-package (synonym-stream-symbol *standard-output*))) "SWANK"))

(defparameter *headless* '((:chrome-options . ((:args . #("--headless"))))))
(setf *timeout* 5)
