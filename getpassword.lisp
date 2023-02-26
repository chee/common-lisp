(defpackage :getpassword
  (:import-from :getseq *VOWEL* *CONSONANT* getseq)
  (:use :cl)
  (:export main password))
(in-package :getpassword)

(defparameter *alphabet*
	(concatenate 'list
		*vowel*
		*consonant*))

(defun letter nil
   (char-upcase
      (nth (secure-random:number (length *alphabet*)) *alphabet*)))

(defun digit nil
  (digit-char (secure-random:number 10)))

(defun password (length)
	(let
      ((special-place (secure-random:number length))
         password)
      (dotimes (n length)
         (when (= n special-place)
            (push (concatenate 'string
                     (list (letter) (digit)))
               password))
         (push (getseq 1) password))
     (format nil "~{~A~^-~}" password)))


(defun main (&rest _)
  ;; buildapp compatability
  (declare (ignore _))
  (let ((length (first (uiop:command-line-arguments))))
    (princ
      (password
        (if length (parse-integer length) 5))))
  (fresh-line))
