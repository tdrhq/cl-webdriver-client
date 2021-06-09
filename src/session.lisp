(in-package :webdriver-client)

(defvar *session* nil "The current Selenium WebDriver session.")

(defclass session ()
  ((id :initarg :id
       :initform (error "Must supply an id")
       :reader session-id))
  (:documentation "A Selenium Webdriver session.

The server should maintain one browser per session. Commands sent to a session will be directed to the corresponding browser."))

(defmethod print-object ((session session) stream)
  (print-unreadable-object (session stream :type t :identity t)
    (write-string (session-id session) stream)))

(defun session-path (session fmt &rest args)
  (format nil "/session/~a~a" (session-id session) (apply #'format nil fmt args)))

(defun make-session (&optional capabilities)
  "Creates a new WebDriver session with the endpoint node. If the creation fails, a session not created error is returned.

CAPABILITIES are the capabilities to negotate for the new session. If it is NIL, then *DEFAULT-CAPABILITIES* are used. If it is a list, then it is use as parameters for MAKE-CAPABILITIES to build a new CAPABILITIES object. Otherwise, it is assumed to be a CAPABILITIES object.

Category: Session
See: https://www.w3.org/TR/webdriver1/#new-session .
See: https://www.w3.org/TR/webdriver1/#capabilities ."
  (let ((caps (cond
		((null capabilities)
		 *default-capabilities*)
		((listp capabilities)
		 (apply #'make-capabilities capabilities))
		(t capabilities))))
    (check-type caps capabilities)
    (let ((response (http-post "/session"
                               :session-id nil
			       :capabilities (serialize-capabilities caps))))
      (make-instance 'session
                     :id (aget (aget response :value) :session-id)))))

(defun delete-session (session)
  "Delete the WebDriver SESSION.

Category: Session"
  (http-delete-check (session-path session "")))

(defun use-session (session)
  "Make SESSION the current session.

Category: Session"
  (setf *session* session))

(defmacro with-session (capabilities &body body)
  "Execute BODY inside a Selenium session.

Category: Session
See: MAKE-SESSION"
  (with-gensyms (session)
    `(let (,session)
       (unwind-protect
            (progn
              (setf ,session (make-session ,capabilities))
              (let ((*session* ,session))
                ,@body))
         (when ,session
           (delete-session ,session))))))

(defun start-interactive-session (&optional capabilities)
  "Start an interactive session. Use this to interact with Selenium driver from a REPL.

Category: Session
See: MAKE-SESSION"
  (when *session*
    (delete-session *session*))
  (setf *session* (make-session capabilities)))

(defun stop-interactive-session ()
  "Stop an interactive session.

Category: Session"
  (when *session*
    (delete-session *session*)
    (setf *session* nil)))
