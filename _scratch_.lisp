(in-package :common-lisp-user)

(defpackage "APP"
  (:use :clim :clim-lisp)
  (:export "APP-MAIN"))

(in-package :app)

(define-application-frame superapp ()
  ()
  (:panes
   (int :interactor :height 400 :width 600))
  (:layouts
   (default int)))

(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp)))

(defclass animal ()
  ((name :initarg :name)
    (size :initarg :size)
    kind sound))

(defclass cat (animal)
  ((kind :initform '🐈‍⬛)
    (sound :initform "meow")))

(defclass dog (animal)
  ((kind :initform '🐕)
    (sound :initform "wof")))

(defclass toy ()
  ((kind :initarg :kind)))

(defvar *ball* (make-instance 'toy :kind '🎾))

(defvar *rover*
  (make-instance 'dog :name "Rover" :size "Small"))

(defvar *stella*
  (make-instance 'cat :name "Stella" :size "Medium"))
(defun u-escape (text start end)
  (print text)
  (or (let ((sub (subseq text start end))
        (capture nil)
        (position start))
   (when (char= (first sub) #\u)
     (when (hexitp (second sub))
       (incf position)
       (incf position)
       (setf capture `(#\u ,(second sub)))
       (loop for i from position upto (+ position 5)
         do (let ((char (nth position text)))
              (cond
                ((hexitp char)
                  (setf capture (append capture char))
                  (incf position))
                (t (return)))))
       (values capture position))))
    (values nil nil)))

(defun hexitp (char)
  (member char
    '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
     #\a #\b #\c #\d #\e #\f
       #\A #\A #\B #\C #\D #\E #\F)))

(defun code-chars (&rest codes)
  (loop for code in codes collect
    (typecase code
      (cons (apply 'code-chars code))
      (integer (code-char code)))))
