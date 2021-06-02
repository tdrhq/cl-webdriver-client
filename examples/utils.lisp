(defpackage my-test
  (:use :cl :webdriver-client)
  (:import-from
   :webdriver-client-utils
   :send-keys
   :click
   :wait-for))

(in-package :my-test)

(with-session ()
  (setf (url) "http://google.com")
  (send-keys "cl-webdriver-client")
  (click "[name=btnG]")
  (wait-for "#resultStats"))
