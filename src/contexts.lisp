(in-package :webdriver)

;; Many WebDriver commands happen in the context of either the current browsing context or current top-level browsing context. The current top-level browsing context is represented in the protocol by its associated window handle. When a top-level browsing context is selected using the Switch To Window command, a specific browsing context can be selected using the Switch to Frame command.

;; See: https://www.w3.org/TR/webdriver1/#command-contexts

(defun switch-to-window (window-handle &key (session *session*))
  "Switching window will select the current top-level browsing context used as the target for all subsequent commands. In a tabbed browser, this will typically make the tab containing the browsing context the selected tab.

WINDOW-HANDLE is the handle of the window obtained via GET-WINDOW-HANDLE.

See: https://www.w3.org/TR/webdriver1/#switch-to-window
Category: Contexts"
  (http-post-value (session-path session "/window") :handle window-handle))

(defun close-window (&key (session *session*))
  "Close the current window.

See: https://www.w3.org/TR/webdriver1/#close-window
Category: Contexts"
  (http-delete (session-path session "/window")))

(defun get-window-handle (&key (session *session*))
  "Returns the window handle associated with the current top-level browsing context.
See: https://www.w3.org/TR/webdriver1/#get-window-handle
Category: Contexts"
  (http-get-value (session-path session "/window")))

(defun get-window-handles (&key (session *session*))
  "Returns the window handles associated with ech top-level browsing context.
See: https://www.w3.org/TR/webdriver1/#get-window-handles
Category: Contexts"
  (http-get-value (session-path session "/window/handles")))

(defun switch-to-frame (id &key (session *session*))
  "Change focus to another frame on the page. If the frame id is null, the server
should switch to the page's default content.

In the context of a web browser, a frame is a part of a web page or browser window which displays content independent of its container, with the ability to load content independently.

Category: Contexts
See: https://www.w3.org/TR/webdriver1/#switch-to-frame"
  (http-post-check (session-path session "/frame")
                   :id id))

(defun new-window (&key (session *session*))
  "Create a new top-level browsing context. 

Category: Contexts
See: https://w3c.github.io/webdriver/#new-window"
  (http-post-value (session-path session "/window/new")))
