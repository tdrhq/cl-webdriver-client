#/bin/sh

sbcl --non-interactive \
     --eval '(ql:quickload :cl-webdriver-client)' \
     --eval '(asdf:test-system :cl-webdriver-client)'
