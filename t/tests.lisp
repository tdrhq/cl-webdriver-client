(in-package :cl-selenium-test)

(defparameter +test-pages-filepath+ (asdf:system-relative-pathname :cl-selenium-test "t/testpages.tar.gz"))

(defun download-test-pages ()
  (format t "Downloading test pages...~%")
  (uiop:run-program (list "curl" "-L" "https://github.com/copyleft/cl-selenium-webdriver/files/6430883/testpages.tar.gz"
		    "--output"
		    (princ-to-string +test-pages-filepath+))))

(defun uncompress-test-pages ()
  (format t "Uncompressing test pages...~%")
  (uiop:run-program
   (format nil "tar xzf ~a -C ~a"
	   +test-pages-filepath+
	   (asdf:system-relative-pathname :cl-selenium-test "t/"))))  

(defun test-file-url (name)
  (format nil "file://~a" (asdf:system-relative-pathname :cl-selenium-test (format nil "t/web/~a" name))))

(defmacro with-test-session (&body body)
  `(with-session (:additional-capabilities *headless*)
     ,@body))

(unless (probe-file (asdf:system-relative-pathname :cl-selenium-test (format nil "t/web/")))
  (unless (probe-file +test-pages-filepath+)
    (download-test-pages))
  (uncompress-test-pages))

(plan nil)

(subtest "Basic test"
  (with-test-session ()
    (setf (url) (test-file-url "xhtmlTest.html"))
    (ok (find-element "[name='someForm']"))))

;; Visibility tests

(subtest "Visibility test 1"
  (with-test-session ()
    (setf (url) (test-file-url "javascriptPage.html"))
    (ok (element-displayed (find-element "displayed" :by :id)))
    (ok (not (element-displayed (find-element "none" :by :id))))
    (ok (not (element-displayed (find-element "suppressedParagraph" :by :id))))
    (ok (not (element-displayed (find-element "hidden" :by :id))))
    ))

(subtest "Visibility test 2"
  (with-test-session ()
    (setf (url) (test-file-url "javascriptPage.html"))
    (let ((child-div (find-element "hiddenchild" :by :id))
	  (hidden-link (find-element "hiddenlink" :by :id)))
      (ok (not (element-displayed child-div)))
      (ok (not (element-displayed hidden-link)))
      )))

;; Typing

(subtest "Should fire key press events"
  (with-test-session ()
    (setf (url) (test-file-url "javascriptPage.html"))
    (let ((key-reporter (find-element "keyReporter" :by :id)))
      (element-send-keys key-reporter "a")
      (let ((result (find-element "result" :by :id)))
	(ok (search "press:" (element-text result) :test 'string=))))))


(subtest "Writable text input should clear"
  (with-test-session ()
    (setf (url) (test-file-url "readOnlyPage.html"))
    (let ((element (find-element "writableTextInput" :by :id)))
      (element-clear element)
      (ok (string= (element-attribute element "value") "")))))

(subtest "Text input should not clear when disabled"
  (with-test-session ()
    (setf (url) (test-file-url "readOnlyPage.html"))
    (let ((element (find-element "textInputnotenabled" :by :id)))
      ;; assert element is enabled here
      (is-error (element-clear element) 'error))))

;; Click

(subtest "Can click on a link that overflows and follows it"
  (with-test-session ()
    (setf (url) (test-file-url "clicks.html"))
    (element-click (find-element "overflowLink" :by :id))
    (sleep 2)
    (ok (string= (page-title) "XHTML Test Page"))))

(finalize)

