(in-package :webdriver)

(defun dismiss-alert (&key (session *session*))
  "The Dismiss Alert command dismisses a simple dialog if present. A request to dismiss an alert user prompt, which may not necessarily have a dismiss button, has the same effect as accepting it.

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
  "The Send Alert Text command sets the text field of a window.prompt user prompt to the given value.

Category: User prompts
See: https://www.w3.org/TR/webdriver1/#send-alert-text"
  (http-post-value (session-path session "/alert/text") :text text))
