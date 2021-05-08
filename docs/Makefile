.PHONY: all clean

all: cl-selenium-webdriver.info cl-selenium-webdriver.html cl-selenium-webdriver.pdf index.html

cl-selenium-webdriver.info: cl-selenium-webdriver.weaved.texi
	makeinfo --no-validate cl-selenium-webdriver.weaved.texi

cl-selenium-webdriver.pdf: cl-selenium-webdriver.weaved.texi
	texi2any --force --no-validate --pdf cl-selenium-webdriver.weaved.texi -o cl-selenium-webdriver.pdf

cl-selenium-webdriver.html: cl-selenium-webdriver.weaved.texi
	texi2any --no-validate --html cl-selenium-webdriver.weaved.texi --css-ref texinfo-klare.css --no-split -o cl-selenium-webdriver.html

index.html: cl-selenium-webdriver.weaved.texi
	texi2any --no-validate --html cl-selenium-webdriver.weaved.texi --css-ref texinfo-klare.css --no-split -o index.html

cl-selenium-webdriver.weaved.texi:
	sbcl --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "cl-selenium-webdriver.texi" "cl-selenium-webdriver.weaved.texi" :docsystem :texinfo :modules (list :cl-selenium))' --quit

clean:
	rm -f *.weaved*
	rm -f *.pdf
	rm -f *.html
