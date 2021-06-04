(in-package :webdriver-client)

(defun detect-platform-name ()
  (string-downcase (symbol-name (uiop/os:operating-system))))

(defun make-capabilities (always-match &rest first-match)
  "Helper function for creating capabilities.

Category: Capabilities"
  `((:always-match . ,always-match)
    (:first-match . ,first-match)))

(defun merge-capabilities (cap1 cap2)
  "Merge two capabilities

Category: Capabilities"
  `((:always-match . ,(append (aget cap1 :always-match)
                              (aget cap2 :always-match)))
    (:first-match . ,(append (aget cap1 :first-match)
                             (aget cap2 :first-match)))))

(defparameter *default-capabilities*
  (make-capabilities
   `((platform-name . ,(detect-platform-name)))
   '((browser-name . "chrome"))
   '((browser-name . "firefox")))
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

(firefox-capabilities :args (list \"--headless\"))

Category: Capabilities
See: https://developer.mozilla.org/en-US/docs/Web/WebDriver/Capabilities/firefoxOptions"
  `("moz:firefoxOptions" . ,(alexandria:plist-alist options)))
