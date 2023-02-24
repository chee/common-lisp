(defsystem #:getpassword
	:author "chee <yay@chee.party>"
	:version "1"
	:build-operation "program-op"
	:build-pathname "getpassword"
	:entry-point "getpassword:main"
	:depends-on (#:getseq #:secure-random)
	:components ((:file "getpassword")))
