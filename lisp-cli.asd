(asdf:defsystem #:lisp-cli
	:author "chee <yay@chee.party>"
	:version "0"
	:build-operation "program-op"
	:build-pathname "lisp"
	:entry-point "lisp-cli:main"
	:depends-on (#:str #:uiop #:quicklisp)
	:components ((:file "lisp-cli")))
