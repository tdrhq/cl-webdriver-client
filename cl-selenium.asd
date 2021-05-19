(asdf:defsystem #:cl-selenium
  :description "cl-selenim-webdriver is a binding library to the Selenium 4.0"
  :author "TatriX <tatrics@gmail.com>"
  :license "MIT"
  :depends-on (:dexador :quri :cl-json :alexandria :split-sequence :assoc-utils)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "errors")
                 (:file "session")
		 (:file "cookie")
                 (:file "keys")
                 (:file "mouse")
		 (:file "user-prompts")
                 (:file "http")
                 (:file "selenium")
		 (:file "utils"))))
  :in-order-to ((test-op (test-op cl-selenium-test))))
