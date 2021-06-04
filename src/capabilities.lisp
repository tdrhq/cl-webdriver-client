(in-package :webdriver-client)

(defun detect-platform-name ()
  (string-downcase (symbol-name (uiop/os:operating-system))))

(defun make-capabilities (always-match &rest first-match)
  "Helper function for creating capabilities.

ALWAYS-MATCH is an alist with capabilities parameters:

- :browser-name -- a string. The name of the browser to use.
- :browser-version -- a string. The browser version required.
- :platform-name -- a string. Identifies the operating system of the endpoint node. e.g. 'Linux', 'Windows'.
- :page-load-strategy -- a string. The page load strategy. 
- :accept-insecure-certs -- a boolean. Whether expired or invalid TLS certificates are checked when navigating.
- :timeouts -- an alist. Describes the timeouts imposed on certain session operations. 

FIRST-MATCH is a list of alists, with the alists being the capabilities to try as 'firstMatch'.

See: https://www.w3.org/TR/webdriver1/#capabilities
See: https://developer.mozilla.org/en-US/docs/Web/WebDriver/Capabilities#capabilities_negotiation
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
