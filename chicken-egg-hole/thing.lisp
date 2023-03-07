(in-package :chicken-egg-hole)

(defclass thing nil
  ((name
     :reader thing-name
     :initarg :name
     :initform (error "Things must have names!"))
    ;; TODO icon should be a class property
    (icon
      :reader thing-icon
      :initarg :icon
      :initform (error "Things must have a single-character icon symbol."))
    (point
      :accessor thing-point
      :initarg :point
      :initform (make-point 0 0))))

(defmethod print-object ((thing thing) out)
  (print-unreadable-object (thing out :type t)
    (format out "(~s ~s)"
      (thing-row thing)
      (thing-column thing))))

(defmacro defthing (name icon-name)
  (let* ((lowname (string-downcase (symbol-name name)))
          (upname (string-upcase lowname))
          (constructor (intern (format nil "MAKE-~a" upname)))
          (predicate (intern (format nil "~a?" upname)))
          (icon (intern (symbol-name icon-name) :keyword)))
    `(progn
       (defclass ,name (thing)
         ((name :initform ,lowname)
           (icon :initform ',icon)))
       (defun ,constructor (&optional (row 0) (column 0))
         (make-instance ',name
           :point (make-point row column)))
       (defun ,predicate (thing) (typep thing ',name)))))

(defmethod thing-column ((thing thing))
  (point-column (thing-point thing)))
(defmethod thing-row ((thing thing))
  (point-row (thing-point thing)))
(defmethod thing-at? ((thing thing) (point point))
  (points-equal? point (thing-point thing)))
(defmethod thing-on? ((a thing) (b thing))
  (points-equal? (thing-point a) (thing-point b)))
