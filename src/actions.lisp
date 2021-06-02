(in-package :webdriver)

(defun expand-action-item (action-item)
  (destructuring-bind (action-type &rest args) action-item
    (case action-type
      (:pause `((:type . ,"pause")
                (:duration . ,(first args))))
      (:pointer-move (destructuring-bind (x y) args
                       `((:type . "pointerMove")
                         (:x . ,x)
                         (:y . ,y))))
      (:pointer-up `((:type . "pointerUp")
                     (:button . ,(first args))))
      (:pointer-down `((:type . "pointerDown")
                       (:button . ,(first args))))
      (:pointer-cancel `((:type . "pointerCancel")))
      (:key-down `((:type . "keyDown")
                   (:value . ,(first args))))
      (:key-up `((:type . "keyUp")
                 (:value . ,(first args))))
      (:scroll `((:type . "scroll")
                 ,@(alexandria:plist-alist args)))
      (t (error "Invalid action: ~a" action-type)))))

(defun expand-actions (actions)
  (flet ((expand-action-items (action-items)
           (mapcar 'expand-action-item action-items)))
    (loop for action-sequence in actions
          collect
          (destructuring-bind (source-type &rest action-items) action-sequence
            (if (member source-type '(:touch :mouse :pen))
                ;; an special pointer type
                `((:id . ,(princ-to-string (gensym)))
                  (:type . "pointer")
                  (:parameters . ((:pointer-type . ,(string-downcase (symbol-name source-type)) )))
                  (:actions . ,(expand-action-items action-items)))
                ;; else
                `((:id . ,(princ-to-string (gensym)))
                  (:type . ,source-type)
                  (:actions . ,(expand-action-items action-items))))))))

;; (expand-actions '((:pointer (:pointer-move 22 33))))
;; (expand-actions '((:pointer (:pointer-move 22 33))
;;                   (:none (:pause 22))))
;; (expand-actions `((:key
;; 		   (:pause 1000)
;; 		   (:key-down ,(key :shift))
;; 		   (:key-down "a"))))

(defun perform-actions (actions &optional (session *session*))
  "The Actions API provides a low-level interface for providing virtualised device input to the web browser.
Conceptually, the Actions commands divide time into a series of ticks. The local end sends a series of actions which correspond to the change in state, if any, of each input device during each tick. For example, pressing a key is represented by an action sequence consisting of a single key input device and two ticks, the first containing a keyDown action, and the second a keyUp action, whereas a pinch-zoom input is represented by an action sequence consisting of three ticks and two pointer input devices of type touch, each performing a sequence of actions pointerDown, followed by pointerMove, and then pointerUp.

Category: Actions
See: https://www.w3.org/TR/webdriver/#actions "
  (http-post-check (session-path session "/actions")
                   :actions (expand-actions actions)))
