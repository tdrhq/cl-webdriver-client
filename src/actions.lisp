(in-package :cl-selenium)

(defun expand-action-item (action-item)
  (destructuring-bind (action-type &rest args) action-item
      (case action-type
	(:pause `((:type . ,"pause")
		  (:duration . ,(first args))))
	(:move (destructuring-bind (x y) args
		 `((:type . "pointerMove")
		   (:x . ,x)
		   (:y . ,y))))
	(t (error "Invalid action: ~a" action-type)))))

(defun expand-actions (actions)
  (flet ((expand-action-items (action-items)
	   (mapcar 'expand-action-item action-items)))
    (loop for action-sequence in actions
	  collect
	  (destructuring-bind (source-type &rest action-items) action-sequence
	    `((:id . ,(princ-to-string (gensym)))
	      (:type . ,source-type)
	      (:actions . ,(expand-action-items action-items)))))))

(expand-actions '((:pointer (:move 22 33))))
(expand-actions '((:pointer (:move 22 33))
		  (:none (:pause 22))))

(defun perform-actions (actions &optional (session *session*))
  (http-post-check (session-path session "/actions")
		   :actions (expand-actions actions)))

