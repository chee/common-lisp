(defpackage #:cloghole
  (:use #:cl #:clog)
  (:export start))

(in-package :cloghole)

(defun on-click (obj)
  (setf (text obj) "DEAD")
  (setf (connection-data-item obj "done") t)
  (set-on-click obj nil))

(defvar *total-clicks* 0)

(defun count-string (name count)
  (declare
    (type string name)
    (type number count))
  (format nil "~A: ~A" name count))
(defun total-string (count)
  (declare (type number count))
  (count-string "total" count))
(defun click-string (count)
  (declare (type number count))
  (count-string "clicks" count))

(defun on-new-window (body)
  (handler-case
    (progn
      (setf (title (html-document body)) "gwgw")
      (let* ((your-clicks 0)
              (the-button
                (create-button body :content "Click this."))
              (the-total (create-p body
                           :content (total-string *total-clicks*)))
              (the-clicks (create-p body
                            :content (click-string your-clicks))))
        (set-on-click the-button
          (lambda (event)
            (progn
              (incf *total-clicks*)
              (incf your-clicks)
              (setf (text the-total) (total-string *total-clicks*))
              (setf (text the-clicks) (click-string your-clicks)))))))
    (error (c)
      (format t "Lost connection.~%~%~A" c))))

(defun start ()
  "Start."
  (initialize 'on-new-window)
  (open-browser))
