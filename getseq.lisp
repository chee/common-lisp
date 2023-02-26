(defpackage :getseq
	(:use :cl)
	(:export *vowel* *consonant* getseq main))

(in-package :getseq)

(defparameter *vowel*
	'(#\a #\e #\i #\o #\u))

(defparameter *consonant*
	'(#\b #\c #\d #\f #\g #\h #\j #\k #\l #\m #\n #\p #\q #\r #\s #\t
		 #\v #\w #\y #\z))

(defun random-item (seq)
	(nth (secure-random:number (length seq)) seq))

(defun consonant nil
	(random-item *consonant*))

(defun vowel nil
	(random-item *vowel*))

(defun word nil
	(list (consonant) (vowel) (consonant) (vowel) (consonant)))

(defun getseq (length)
	(let (seq)
		(dotimes (n length)
			(push (concatenate 'string (word)) seq))
		(format nil "~{~A~^-~}" seq)))

(defun main nil
  (let* ((length-arg (first (uiop:command-line-arguments)))
          (length (if length-arg (parse-integer length-arg) 5)))
    (dotimes (n length)
			(format t (if (= n (- length 1)) "~{~A~}" "~{~A~}-") (word)))
	  	  (fresh-line)))
