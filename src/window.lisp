(in-package :webdriver-client)

(defun set-window-rect (&key x y width height (session *session*))
  (http-post-check
   (session-path session "/window/rect")
   :x x
   :y y
   :width width
   :height height))

(defun get-window-rect (&key (session *session*))
  (let ((alist (http-get-value
                (session-path session "/window/rect"))))
    (list
     (assoc-value alist :x)
     (assoc-value alist :y)
     (assoc-value alist :width)
     (assoc-value alist :height))))

(defun window-resize (&key width height (session *session*))
  (destructuring-bind (x y old-width old-height) (get-window-rect :session session)
    (set-window-rect
     x y
     (or width old-width)
     (or height old-height))))
