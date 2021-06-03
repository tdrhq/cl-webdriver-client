(asdf:defsystem #:cl-webdriver-client
  :description "cl-webdriver-client is a binding library to the Selenium 4.0"
  :author ("TatriX <tatrics@gmail.com>" "Mariano Montone <marianomontone@gmail.com>")
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
		 (:file "actions")
                 (:file "http")
                 (:file "webdriver")
		 (:file "utils"))))
  :in-order-to ((test-op (test-op cl-webdriver-client-test))))
