(in-package :cl-selenium-test)

(defun download-test-pages ()
  (uiop:run-program (list "curl" "-L" "https://github.com/copyleft/cl-selenium-webdriver/files/6430883/testpages.tar.gz"
		    "--output"
		    (princ-to-string (asdf:system-relative-pathname :cl-selenium-test "t/testpages.tar.gz")))))

(defun uncompress-test-pages ()
  (uiop:run-program
   (format nil "tar xzf ~a -C ~a"
	   (asdf:system-relative-pathname :cl-selenium-test "t/testpages.tar.gz")
	   (asdf:system-relative-pathname :cl-selenium-test "t/"))))  

(defun test-file-url (name)
  (format nil "file://~a" (asdf:system-relative-pathname :cl-selenium-test (format nil "t/web/~a" name))))

(defmacro with-test-session (&body body)
  `(with-session (:additional-capabilities *headless*)
     ,@body))

(subtest "Basic test"
  (with-test-session ()
    (setf (url) (test-file-url "xhtmlTest.html"))
    (ok (find-element "[name='someForm']"))))

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

