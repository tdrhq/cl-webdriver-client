(in-package :webdriver-client)

(defun mouse-move-to (x y &key element (session *session*))
  "Move the mouse by an offset of the specificed element. If no element is specified, the move is relative to the current mouse cursor. If an element is provided but no offset, the mouse will be moved to the center of the element. If the element is not visible, it will be scrolled into view.

See: https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol#sessionsessionidmoveto"
  (http-post-check (session-path session "/moveto")
                   :element (when element (element-id element))
                   :xoffset x
                   :yoffset y))

(defun mouse-click (button &key (session *session*))
  "Click any mouse button (at the coordinates set by the last moveto command). Note that calling this command after calling buttondown and before calling button up (or any out-of-order interactions sequence) will yield undefined behaviour).

See: https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol#sessionsessionidclick"
  (http-post-check (session-path session "/click")
                   :button (ecase button
			     (:left 0)
			     (:middle 1)
			     (:right 2))))
