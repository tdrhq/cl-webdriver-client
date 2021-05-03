(in-package :cl-selenium-utils)

(defparameter *timeout* 30
  "Default timeout value to use in selenium-utils functions.")

(defparameter *default-element-func*
  #'active-element
  "Function used to get the 'default element' by selenium-utils functions.
It is ACTIVE-ELEMENT function by default.")

(defun find-elem (selector &key (by :css-selector))
  "Find element by SELECTOR. Returns NIL if the element is not found."
  (first (find-elements selector :by by)))

(defun wait-for (selector &key (timeout *timeout*))
  "Wait for an element that matches SELECTOR to appear on the screen.
TIMEOUT indicates how much time to wait (default is *TIMEOUT*)."
  (loop
     for i from 0
     for elem = (find-elem selector)
     until elem
     if (= i (* 2 timeout))
     do (error "Element ~a didn't appear" selector)
     else
     do (sleep 0.5)
     finally (return elem)))

(defun get-cookie (cookie name)
  "Get value of COOKIE at NAME."
  (assoc-value (find name
                     cookie
                     :test #'equal
                     :key (rcurry #'assoc-value :name))
               :value))

(defun elem (&optional selector)
  "If SELECTOR is given, wait for an element that matches the selector to appear.
Otherwise, call *DEFAULT-ELEMENT-FUNC* (the active element is returned by default)."
  (if selector
      (wait-for selector)
      (funcall *default-element-func*)))

(defun attr (name &optional selector)
  "Get acttive element attribute."
  (element-attribute (elem selector) name))

(defun id (&optional selector)
  "Get active element id."
  (attr "id" selector))

(defun classname (&optional selector)
  "Get active element classname."
  (attr "className" selector))

(defun classlist (&optional selector)
  "Get active element class list."
  (split-sequence:split-sequence #\Space (classname selector)))

(defun text (&optional selector)
  "Get active element's text."
  (element-text (elem selector)))

(defun send-key (key &optional selector)
  "Send a key to active element."
  (element-send-keys (elem selector) (key key)))

(defun send-keys (keys &optional selector)
  "Send keys to active element."
  (element-send-keys (elem selector) keys))

(defun click (&optional selector)
  "Click on active element."
  (element-click (elem selector)))
