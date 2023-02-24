(defpackage :lisp-cli
  (:use :cl)
  (:export main))
(in-package :lisp-cli)

(defvar *commands* nil)

(defcommand help
  "You're looking at it!"
  (declare (ignore arguments))
  (princ "
Lisp CLI.
=========
by rabbits for rabbits
----------------------

Commands:
")
  (loop for (name fn) on *commands* by #'cddr while fn
    do (format t " lisp ~A ~A~%" (string-downcase name) (documentation fn 'function))))

(defmacro defcommand (command-name &rest body)
  (let ((command-fn
          (intern (concatenate 'string
            "CLI-COMMAND-"
            (symbol-name command-name)))))
    `(prog1
       (defun ,command-fn (&rest arguments)
         ,@body)
       (setf (getf *commands* ,(intern (symbol-name command-name) :keyword)) #',command-fn))))

(defcommand build
  	"<system> - Run `asdf:make' on SYSTEM"
  (let* ((system-string (first arguments))
          (system
            (intern (string-upcase system-string) :keyword)))
    (unless system-string
      (cli-command-help)
      (fresh-line uiop:*stderr*)
      (princ "No system name provided to build command!" uiop:*stderr*)
      (fresh-line uiop:*stderr*)
      (uiop:quit 1))
    (ql:quickload system)
    (asdf:make system)))

(defcommand ql
  	"<package> - Install package with quicklisp"
  (let* ((package-string (first arguments))
          (package
            (intern (string-upcase package-string) :keyword)))
    (unless package-string
      (cli-command-help)
      (fresh-line uiop:*stderr*)
      (princ "No package name provided to ql command!" uiop:*stderr*)
      (fresh-line uiop:*stderr*)
      (uiop:quit 1))
    (ql:quickload package)))


(defcommand eval
  "<expression> [expression...] - Evaluate some lisp (quietly) with sbcl."
  (uiop:run-program
    `("sbcl"
       "--noinform"
       "--no-userinit"
       ,@(append (mapcar (lambda (form) (list "--eval" form)) arguments))
       "--quit")
    :output :interactive))

(defcommand run
  "<file> [file...] - Load and run lisp files (quietly) with sbcl"
  (uiop:run-program
    `("sbcl"
       "--noinform"
       "--no-userinit"
       ,@(append (mapcar (lambda (form) (list "--load" form)) arguments))
       "--quit")
    :output :interactive))

(defun main nil
  (let* ((argv (uiop:command-line-arguments))
          (command-name (first argv))
          (arguments (rest argv))
          (command (getf *commands* (intern (string-upcase command-name) :keyword))))
    (if command (apply command arguments) (cli-command-help))))
