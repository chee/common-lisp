(defpackage :lisp-cli
  (:use :cl)
  (:export main))
(in-package :lisp-cli)

(defun lisp-help (&rest args)
  (declare (ignore args))
  (princ "
Lisp CLI.
=========
by rabbits for rabbits
----------------------

Commands:

	lisp compile <SYSTEM> - Run `asdf:make' on SYSTEM
	lisp eval '<LISP-FORMS>' - Evaluate some lisp (quietly) with sbcl
"))

(defun lisp-compile (&rest args)
  (let* ((system-string (first args))
          (system (intern (string-upcase system-string) :keyword)))
    (ql:quickload system)
    (asdf:make system)))

(defun lisp-eval (&rest args)
  (uiop:run-program
    `("sbcl"
       "--noinform"
       "--no-userinit"
       "--eval" ,(first args)
       "--quit")
    :output :interactive))

(defun lisp-run (&rest args)
  (uiop:run-program
    `("sbcl"
       "--noinform"
       "--no-userinit"
       "--eval" ,(first args)
       "--quit")
    :output :interactive))

(defun main nil
  (let* ((arguments (uiop:command-line-arguments))
          (command (first arguments))
          (sub-arguments (rest arguments)))
    (apply
      (cond
        ((string= command "compile") #'lisp-compile)
        ((string= command "eval") #'lisp-eval)
        (t #'lisp-help))
      sub-arguments)))
