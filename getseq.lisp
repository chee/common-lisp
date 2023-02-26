(defpackage :getseq
	(:use :cl)
	(:export *seed* *vowel* *consonant* random-item random-number make-fresh-seed getseq main))
(in-package :getseq)

(defvar *seed* (isaac:init-kernel-seed :IS64 t))

(defun random-number (max)
  (let* ((max (- max 1))
          (number (isaac:rand-bits-64 *seed* (integer-length max))))
    (if (> number max)
      (random-number max)
      number)))

(defparameter *vowel*
	'(#\a #\e #\i #\o #\u))

(defparameter *consonant*
	'(#\b #\c #\d #\f #\g #\h #\j #\k #\l #\m #\n #\p #\q #\r #\s #\t
		 #\v #\w #\y #\z))

(defun random-item (seq)
	(nth (random-number (length seq)) seq))

(defun consonant nil
	(random-item *consonant*))

(defun vowel nil
	(random-item *vowel*))

(defun word nil
	(list (consonant) (vowel) (consonant) (vowel) (consonant)))

(defun getseq (length &key (stream *standard-output*))
  (dotimes (n length)
      (let* ((end (- length 1))
              (at-end (= n end))
              ;; don't include a hyphen at the end
              (format-string (if at-end "~{~A~}" "~{~A~}-")))
	      (format stream format-string (word)))))

(defun make-fresh-seed nil (isaac:init-kernel-seed :IS64 t))

(defun main nil
  (let ((length (first (uiop:command-line-arguments)))
         (*seed* (make-fresh-seed)))
    (getseq (if length (parse-integer length) 5))
	  (fresh-line)))
