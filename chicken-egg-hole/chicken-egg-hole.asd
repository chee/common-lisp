(defsystem #:chicken-egg-hole
	:author "chee <yay@chee.party>"
	:version "1"
	:depends-on (#:trivial-raw-io)
	:components ((:file "packages")
						(:file "point")
						(:file "thing")
						(:file "game")))

(defsystem #:chicken-egg-hole/sdl
	:author "chee <yay@chee.party>"
	:version "1"
	:build-operation "program-op"
	:build-pathname "chicken-egg-hole-sdl"
	:entry-point "chicken-egg-hole:run"
	:depends-on (#:sdl2 #:sdl2-image #:chicken-egg-hole)
	:components ((:file "sdl")))

(defsystem #:chicken-egg-hole/cli
	:author "chee <yay@chee.party>"
	:version "1"
	:build-operation "program-op"
	:build-pathname "chicken-egg-hole"
	:entry-point "chicken-egg-hole:run"
	:depends-on (#:chicken-egg-hole #:trivial-raw-io)
	:components  ((:file "cli")))
