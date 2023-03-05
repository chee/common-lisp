(defpackage :copy-op1-block
	(:use :cl)
	(:export copy-op1-block main))
(in-package :copy-op1-block)
(defconstant +null+ (code-char 0))
(defun vecstring (string)
  (map 'vector #'char-code string))
(defconstant +op1-json-start+ (vecstring "op-1"))
(defconstant +op1-json-end+ #\space)
(defconstant +ssnd-start+ (vecstring "SSND"))

(defvar
  *_input-file_*
  #p"/Users/chee/db/quiet party/op1/drum/piano/bottompiano-t.aif")

(defvar
  *_output-file_*
  #p"/Users/chee/db/quiet party/op1/drum/piano/toppiano.aif")

(defun word (stream)
  (let ((word (make-array 4
                :element-type
                '(unsigned-byte 8)
                :initial-element 0)))
    (read-sequence word stream)
    word))

(defun read-words-until (terminator stream)
  (let ((count 0))
    (loop
      until (equalp (word stream) terminator)
      do (incf count))
    count))

(defun read-string-until (terminator stream)
  (with-output-to-string (output)
    (loop for char = (code-char (read-byte stream))
      until (char= char terminator)
      do (write-char char output))))

(defun write-string-as-bytes (string stream)
  (write-sequence (map 'list #'char-code string) stream))

(defun read-op1-json-block (source)
  (let (op1-json)
    (with-open-file
      (source-stream source
        :DIRECTION :input
        :ELEMENT-TYPE '(unsigned-byte 8))
      (read-words-until +op1-json-start+ source-stream)
      (setf op1-json (read-string-until +op1-json-end+ source-stream)))
    op1-json))

(defun copy-block (source target)
  (let ((op1-json (read-op1-json-block source)))
    (with-open-file
      (target-stream target
        :DIRECTION :io
        :IF-EXISTS :overwrite
        :ELEMENT-TYPE '(unsigned-byte 8))
      (read-words-until +op1-json-start+ target-stream)
      (write-string-as-bytes op1-json target-stream)
      ;; lol, i should grab the length block before "op-1"
      ;; the aifc spec explains it's `APPL` then a 32-bit int for the
      ;; length. but, the length is always 1004 in these op-1 blocks
      (loop
        until (peek-char #\S)
        do (write-byte \space)))))
