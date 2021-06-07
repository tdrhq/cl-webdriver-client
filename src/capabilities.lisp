(in-package :webdriver-client)

(defun detect-platform-name ()
  (string-downcase (symbol-name (uiop/os:operating-system))))

(defstruct capabilities
  always-match first-match)

(defun merge-capabilities (cap1 cap2)
  "Merge two capabilities

Category: Capabilities"
  (make-capabilities
   :always-match (append (capabilities-always-match cap1)
                         (capabilities-always-match cap2))
   :first-match (append (capabilities-first-match cap1)
			(capabilities-first-match cap2))))

(defparameter *default-capabilities*
  (make-capabilities
   :always-match `((platform-name . ,(detect-platform-name)))
   :first-match (list '((browser-name . "chrome"))
                      '((browser-name . "firefox"))))
  "The default capabilities.

Category: Capabilities")

(defun chrome-capabilities (&rest options)
  "Specifies Chrome specific capabilities.

Category: Capabilities
https://chromedriver.chromium.org/capabilities#h.p_ID_102"
  `("goog:chromeOptions" . ,(alexandria:plist-alist options)))

(defun firefox-capabilities (&rest options)
  "Specify capabilities for Firefox browser.

Example usage:

(firefox-capabilities :args #(\"--headless\"))

Category: Capabilities
See: https://developer.mozilla.org/en-US/docs/Web/WebDriver/Capabilities/firefoxOptions"
  `("moz:firefoxOptions" . ,(alexandria:plist-alist options)))

(defun serialize-capabilities (capabilities)
  `(,@(when (capabilities-always-match capabilities)
	`((:always-match . ,(capabilities-always-match capabilities))))
    (:first-match . ,(capabilities-first-match capabilities))))
