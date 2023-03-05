(defsystem #:egg
	:author "chee <yay@chee.party>"
	:version "1"
	:build-operation "program-op"
	:build-pathname "getseq"
	:entry-point "egg:main"
	:depends-on (#:sdl2 #:sdl2-image #:alexandria)
	:components ((:file "egg")))
