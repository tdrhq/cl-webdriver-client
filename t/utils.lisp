(defpackage webdriver-client-utils-test
  (:use :cl :webdriver-client :webdriver-client-utils :prove))

(in-package :webdriver-client-utils-test)

(defparameter *test-capabilities*
  (webdriver:make-capabilities
   :always-match `((platform-name . ,(webdriver::detect-platform-name)))
   :first-match (list `((browser-name . "chrome")
                        ,(webdriver:chrome-capabilities
                          :args #("--headless")))
                      `((browser-name . "firefox")
                        ,(webdriver:firefox-capabilities
                          :args #("--headless")))))
  "Capabilities used for test sessions")

(defun test-file-url (name)
  (format nil "file://~a" (asdf:system-relative-pathname :cl-webdriver-client-test (format nil "t/web/~a" name))))

(defmacro with-test-session (&body body)
  `(with-session *test-capabilities*
     ,@body))

(plan nil)

(subtest "find-elem"
  (with-test-session
    (setf (url) "http://google.com")
    (is-type (find-elem "[name=q]") 'webdriver::element)
    (is (find-elem (gensym)) nil)))

(subtest "wait-for"
  (with-test-session
    (setf (url) "http://duckduckgo.com")
    (let ((result-selector "#links"))
      (is-error (find-element result-selector) 'no-such-element-error)
      (element-send-keys (find-element "[name=q]") "cl-webdriver-client")
      (sleep 0.5)
      (element-click (find-element "#search_button_homepage"))
      (ok (wait-for result-selector)))))

(subtest "cookie-get"
  (with-test-session
    (setf (url) "http://duckduckgo.com")
    (setf (cookie) (make-cookie "cl-webdriver-client" "common lisp"))
    (is (get-cookie (cookie) "cl-webdriver-client") "common lisp")))

(subtest "elem"
  (with-test-session
    (setf (url) "http://duckduckgo.com")
    (is (element-id (elem)) (element-id (active-element)))
    (is (element-id (elem "#search_button_homepage"))
	(element-id (find-element "#search_button_homepage")))))

(subtest "attr"
  (with-test-session
    (setf (url) "http://duckduckgo.com")
    (is (attr "name") "q")
    (is (id) "search_form_input_homepage")))

(subtest "id"
  (with-test-session
    (setf (url) "http://duckduckgo.com")
    (is (id) "search_form_input_homepage")))

(subtest "classname"
  (with-test-session
    (setf (url) "http://duckduckgo.com")
    (is (classname) "js-search-input search__input--adv")))

(subtest "classlist"
  (with-test-session
    (setf (url) "http://duckduckgo.com")
    (is (classlist) '("js-search-input" "search__input--adv"))))

(subtest "text"
  (with-test-session
    (setf (url) "http://duckduckgo.com")
    (is (text "#logo_homepage_link") "About DuckDuckGo")))

(subtest "send-key"
  (with-test-session
    (setf (url) "http://duckduckgo.com")
    (send-key :tab)
    (is (id) "search_button_homepage")))

(subtest "send-keys"
  (with-test-session
    (setf (url) "http://duckduckgo.com")
    (send-keys "cl-webdriver-client")
    (is (attr "value") "cl-webdriver-client")))

(subtest "click"
  (with-test-session
    (setf (url) "http://duckduckgo.com")
    (page-source)
    (send-keys "cl-webdriver-client")
    (sleep 2)
    (click "#search_button_homepage")
    (ok (wait-for "#links"))))

(finalize)
