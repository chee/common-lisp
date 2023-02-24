(defpackage :getpassword
  (:use :cl :getseq)
  (:export main))
(in-package :getpassword)

(defparameter *alphabet*
	(concatenate 'list
		*vowel*
		*consonant*))

(defun LETTER nil
   (char-upcase
      (nth (secure-random:number (length *alphabet*)) *alphabet*)))

(defun password (length)
	(let
      ((special-place (secure-random:number length))
         password)
      (dotimes (n length)
         (when (= n special-place)
            (push (concatenate 'string
                     (list (LETTER) (digit-char (secure-random:number 10))))
               password))
         (push (getseq 1) password))
     (format nil "~{~A~^-~}" password)))


(defun main nil
   (let ((length (first (uiop:command-line-arguments))))
      (princ
         (password
            (if length (parse-integer length) 5))))
	(fresh-line))
