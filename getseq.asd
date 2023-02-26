(defsystem #:getseq
	:author "chee <yay@chee.party>"
	:version "1"
	:build-operation "program-op"
	:build-pathname "getseq"
	:entry-point "getseq:main"
	:depends-on (#:cl-isaac)
	:components ((:file "getseq")))
