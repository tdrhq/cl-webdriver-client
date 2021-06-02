.PHONY: all clean

all: cl-webdriver-client.info cl-webdriver-client.html cl-webdriver-client.pdf index.html

cl-webdriver-client.info: cl-webdriver-client.weaved.texi
	makeinfo --no-validate cl-webdriver-client.weaved.texi

cl-webdriver-client.pdf: cl-webdriver-client.weaved.texi
	texi2any --force --no-validate --pdf cl-webdriver-client.weaved.texi -o cl-webdriver-client.pdf

cl-webdriver-client.html: cl-webdriver-client.weaved.texi
	texi2any --no-validate --html cl-webdriver-client.weaved.texi --css-ref texinfo-klare.css --css-ref lisp-coloring.css --no-split -o cl-webdriver-client.html
	sbcl --eval '(require :hyperspec)' --load colorize-lisp-examples.lisp cl-webdriver-client.html

index.html: cl-webdriver-client.weaved.texi
	texi2any --no-validate --html cl-webdriver-client.weaved.texi --css-ref texinfo-klare.css --css-ref lisp-coloring.css --no-split -o index.html
	sbcl --eval '(require :hyperspec)' --load colorize-lisp-examples.lisp index.html

cl-webdriver-client.weaved.texi:
	sbcl --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "cl-webdriver-client.texi" "cl-webdriver-client.weaved.texi" :docsystem :texinfo :modules (list :cl-webdriver-client))' --quit

clean:
	rm -f *.weaved*
	rm -f *.pdf
	rm -f *.html
