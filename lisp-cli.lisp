(defpackage :lisp-cli
  (:use :cl)
  (:export main))
(in-package :lisp-cli)

(defvar *commands* nil)

(defmacro die-unless (condition &rest msg)
  `(unless ,condition (print-help-and-die ,@msg)))

(defmacro defcommand (command-name &rest body)
  "Create a command and add it to *COMMANDS*."
  (let ((command-fn
          (intern (concatenate 'string
            "CLI-COMMAND-"
            (symbol-name command-name)))))
    `(prog1
       (defun ,command-fn (&rest arguments)
         ,@body)
       (setf (getf *commands* ,(intern (symbol-name command-name) :keyword)) #',command-fn))))

;; TODO revisit when you know everything
(defcommand build
  	"<system> - Run `asdf:make' on SYSTEM."
  (let* ((system-string (first arguments))
         (system-indicator
           (intern (string-upcase system-string) :keyword)))
    (die-unless system-string "No system name provided to build command!")

    (ql:quickload system-indicator)
    (unless (depends-on-p (asdf:find-system system-indicator) "QUICKLISP")
      (princ "Unbinding quicklisp.")
      (fresh-line)
      ;; Remove quicklisp from the image
      (loop for x being the external-symbol of "QUICKLISP"
        when (boundp x) do (makunbound x)
        when (fboundp x) do (fmakunbound x)))
    ;; Make sure asdf doesn't try using quicklisp to look things up
    (let ((asdf:*system-definition-search-functions*
            '(asdf/package-inferred-system:sysdef-package-inferred-system-search
               asdf/system-registry:sysdef-central-registry-search
               asdf/system-registry:sysdef-source-registry-search)))
      (asdf:make system-indicator))))


(defcommand ql
  	"<system> - Install system with quicklisp"
  (let* ((system-string (first arguments))
          (system
            (intern (string-upcase system-string) :keyword)))
    (die-unless system-string "No system name provided.")
    (ql:quickload system)))


(defun prefix-args (args prefix)
  (let (list)
    (mapc
     (lambda (arg)
        (setq list (append list (list prefix arg))))
      args)
    list))

(defcommand eval
  "<expression> [expression...] - Evaluate some lisp (quietly) with sbcl."
  (uiop:run-program
    `("sbcl"
       "--noinform"
       ,@(prefix-args arguments "--eval")
       "--quit")
    :output :interactive))

(defun depends-on-p (system dependency)
  (member dependency
    (mapcar #'symbol-name (asdf:component-sideway-dependencies system))
    :test #'string=))


(defcommand run
  "<file> [file...] - Load and run lisp files (quietly) with sbcl."
  (uiop:run-program
    `("sbcl"
       "--noinform"
       ,@(prefix-args arguments "--load")
       "--quit")
    :output :interactive))


(defun print-help-and-die (&rest errors)
  (princ "Lisp CLI.")              (fresh-line)
  (princ "=========")              (fresh-line)
  (princ "by rabbits for rabbits") (fresh-line)
  (princ "----------------------") (fresh-line) (fresh-line)
  (princ "Commands:")              (fresh-line)

  (loop for (name fn) on *commands* by #'cddr while fn
    do (format t " lisp ~A ~A~%" (string-downcase name) (documentation fn 'function)))
  (when errors
    (format uiop:*stderr* "~%")
    (dolist (msg errors) (fresh-line) (princ msg))
    (uiop:quit 7)))

(defcommand help
  "- You're looking at it!"
  (declare (ignore arguments))
  (print-help-and-die))

;; &rest _ for compatability with buildapp
(defun main (&rest _)
  (declare (ignore _))
  (let* ((argv (uiop:command-line-arguments))
          (command-name (first argv))
          (arguments (rest argv))
          (command (getf *commands* (intern (string-upcase command-name) :keyword))))
    (if command (apply command arguments) (cli-command-help))))
