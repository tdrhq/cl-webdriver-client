;;;; package.lisp

(defpackage cl-selenium
  (:use :cl)
  (:export :make-session
           :delete-session
           :with-session
           :use-session

           :start-interactive-session
           :stop-interactive-session

           :key

           :url
           :back
           :refresh
	   :page-title
	   
           :find-element
           :find-elements
           :active-element
           :element-clear
           :element-click
           :element-displayed
           :element-location
           :element-send-keys
           :element-id
           :element-text
           :element-tagname
           :element-attribute

           :switch-to-frame
           :close-current-window

           :mouse-move-to
           :mouse-click

           :make-cookie
           :cookie

           :log-types
           :logs

           :screenshot

           :execute-script

           :no-such-element-error)
  (:import-from :alexandria
                :with-gensyms
                :assoc-value)
  (:documentation "This package exports functions for working with Selenium WebDriver.

For documentation see:
- https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol#sessionsessionidframe
- https://www.w3.org/TR/webdriver1."))

(defpackage cl-selenium-utils
  (:use :cl :cl-selenium)
  (:export :*timeout*
           :*default-element-func*
           :find-elem
           :wait-for
           :get-cookie
           :elem
           :attr
           :id
           :classname
           :classlist
           :text
           :send-key
           :send-keys
           :click)
  (:import-from :alexandria
                :assoc-value
                :rcurry))
