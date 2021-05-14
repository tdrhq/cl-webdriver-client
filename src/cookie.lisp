(in-package :cl-selenium)

(defclass cookie ()
  ((name :initarg :name
	 :documentation "The name of the cookie")
   (value :initarg :value
	  :documentation "The cookie value")
   (path :initarg :path
	 :initform nil
	 :documentation "The cookie path. Defaults to '/' if omitted when adding a cookie. ")
   (domain :initarg :domain
	   :initform nil
	   :documentation "The domain the cookie is visible to. Defaults to the current browsing context’s active document’s URL domain if omitted when adding a cookie.")
   (secure :initarg :secure
	   :initform nil
	   :documentation "Whether the cookie is a secure cookie. Defaults to false if omitted when adding a cookie.")
   (http-only :initarg :http-only
	      :initform nil
	      :documentation "Whether the cookie is an HTTP only cookie. Defaults to false if omitted when adding a cookie.")
   (expiry :initarg :expiry
	   :initform nil
	   :documentation "When the cookie expires, specified in seconds since Unix Epoch. Must not be set if omitted when adding a cookie."))
  (:documentation "A cookie is described in [RFC6265] by a name-value pair holding the cookie’s data, followed by zero or more attribute-value pairs describing its characteristics.

Category: Cookies"))

(defmethod json:encode-json ((cookie cookie) &optional (stream json:*json-output*))
  (with-slots (name value path domain secure expiry) cookie
    (json:with-object (stream)
      (json:encode-object-member "name" name stream)
      (json:encode-object-member "value" value stream)
      (when domain
	(json:encode-object-member "domain" domain stream))
      (when path
	(json:encode-object-member "path" path stream))
      (when secure
	(json:encode-object-member "secure" secure stream))
      (when expiry
	(json:encode-object-member "expiry" expiry stream)))))

(defun make-cookie (name value &key path domain secure expiry)
  "Create a cookie object.

Category: Cookie"
  (make-instance 'cookie
                 :name name
                 :value value
                 :path path
                 :domain domain
                 :secure secure
                 :expiry expiry))

(defun (setf cookie) (cookie &key (session *session*))
  "Create a cookie in the cookie store associated with the active document’s address using cookie name name, cookie value value, and an attribute-value list of the following cookie concepts listed in the table for cookie conversion from data:

If there is an error during this step, return error with error code unable to set cookie. 

See: https://www.w3.org/TR/webdriver1/#dfn-adding-a-cookie ."
  (check-type cookie cookie)
  (http-post-check (session-path session "/cookie") `(:cookie ,cookie)))

(defun cookie (&key (session *session*))
  "Retrieve all cookies visible to the current page.

Category: Cookies
See: https://www.w3.org/TR/webdriver1/#get-all-cookies .
See: https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol#sessionsessionidcookie ."
  (http-get-value (session-path session "/cookie")))

(defun find-cookie (cookie-name &key (session *session*))
  "Retrieve the cookie with name COOKIE-NAME.

Category: Cookies
See: https://www.w3.org/TR/webdriver1/#get-named-cookie"
  (http-get-value (session-path session "/cookie/~a" cookie-name)))

(defun delete-cookie (cookie-name &key (session *session*))
  "Delete the cookie with name COOKIE-NAME.

Category: Cookies
See: https://www.w3.org/TR/webdriver1/#delete-cookie"
  (http-delete (session-path session "/cookie/~a" cookie-name)))

(defun delete-all-cookies (&key (session *session*))
  "Deletes all cookies

Category: Cookies
See: https://www.w3.org/TR/webdriver1/#delete-all-cookies"
  (http-delete (session-path session "/cookie")))
