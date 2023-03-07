(in-package :chicken-egg-hole)

(defclass point ()
  ((row
      :accessor point-row
      :initarg :row
      :initform 0)
    (column
      :accessor point-column
      :initarg :column
      :initform 0)))

(defun make-point (row column)
  (make-instance 'point
    :row row
    :column column))

(defmethod print-object ((point point) out)
  (print-unreadable-object (point out :type t)
    (format out "(~s ~s)" (point-row point) (point-column point))))

(defmethod point-peek ((point point) direction)
  (let* ((row (point-row point))
          (column (point-column point))
          (next-column
            (case direction
              ((:up :down) column)
              (:left (- column 1))
              (:right (+ column 1))))
          (next-row
            (case direction
              ((:left :right) row)
              (:up (- row 1))
              (:down (+ row 1)))))
    (make-point next-row next-column)))

(defmethod point-within? ((point point) rows columns)
  (when
      (and
        (>= (point-row point) 0)
        (>= (point-column point) 0)
        (< (point-row point) rows)
        (< (point-column point) columns))
      t))

(defmethod points-equal? ((a point) (b point))
  (and
    (= (point-row a) (point-row b))
    (= (point-column a) (point-column b))))
