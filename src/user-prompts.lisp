(in-package :cl-selenium)

(defun dismiss-alert (&key (session *session*))
  "Dismiss Alert.

Category: User prompts
See: https://www.w3.org/TR/webdriver1/#dismiss-alert"

  (http-post-check (session-path session "/alert/dismiss")))

(defun accept-alert (&key (session *session*))
  "Accept Alert.

Category: User prompts
See: https://www.w3.org/TR/webdriver1/#dfn-accept-alert"

  (http-post-check (session-path session "/alert/accept")))

(defun alert-text (&key (session *session*))
  "Get Alert Text.

Category: User prompts
See: https://www.w3.org/TR/webdriver1/#get-alert-text"

  (http-get-value (session-path session "/alert/text")))

(defun (setf alert-text) (text &key (session *session*))
  "Send Alert Text.

Category: User prompts
See: https://www.w3.org/TR/webdriver1/#send-alert-text"
  (http-post-value (session-path session "/alert/text") `(:text ,text)))
