(defpackage :getpassword
  (:import-from :getseq *vowel* *consonant* getseq random-number random-item make-fresh-seed)
  (:use :cl)
  (:export main password))
(in-package :getpassword)

(defparameter *alphabet*
	(concatenate 'list
		*vowel*
		*consonant*))

(defun letter nil
   (char-upcase
      (random-item *alphabet*)))

(defun digit nil
  (digit-char (random-number 10)))

(defun password (length &key (stream *standard-output*))
	(let ((special-place (random-number length))
         (special-part (list (letter) (digit))))
    (dotimes (n length)
      (when (= n special-place)
        (format stream "~{~A~}-" special-part))
      (getseq 1 :stream stream)
      (unless (= n (- length 1)) (format stream "-")))))

(defun main nil
  (let ((length (first (uiop:command-line-arguments)))
         (getseq:*seed* (getseq:make-fresh-seed)))
    (password (if length (parse-integer length) 5))
    (fresh-line)))
