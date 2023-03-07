(in-package :chicken-egg-hole)

(defthing chicken <)
(defthing egg o)
(defthing hole X)
(defthing obstacle @)

(defclass world ()
  ((levels
     :reader levels
     :initarg :levels
     :initform (error "A world needs levels. These are row*column 2d arrays."))
    ;; TODO could get this from the array-dimensions of the level
    (rows
      :reader rows
      :initarg :rows
      :initform 6)
    (columns
      :reader columns
      :initarg :columns
      :initform 8)
    (level-number
      :accessor level-number
      :initarg :level-number
      :initform 0)
    (state
      :accessor state)))

(defmethod print-state ((world world) &key
                         (stream *standard-output*)
                         human)
  (fresh-line stream)
  (unless human (princ "(" stream))
  (loop for row from 0 below (rows world) do
    (loop for column from 0 below (columns world) do
      (let ((thing (thing-at world (make-point row column))))
        (when (= column 0)
          (fresh-line stream)
          (unless human (princ "    (" stream)))
        (if thing
          (princ (string-downcase (symbol-name (thing-icon thing))) stream)
          (princ "_" stream))
        (princ " " stream)
        (when (and (not human) (= column (- (columns world) 1)))
          (princ ")" stream)))))
  (unless human (princ ")" stream)))

(defmethod print-object ((world world) out)
  (print-unreadable-object (world out :type t)
    (print-state world :stream out :human t)))

(defmethod current-level ((world world))
  (nth (level-number world) (levels world)))

(defmethod load-level ((world world))
  (setf (state world) nil)
  (let* ((level-spec (current-level world))
          (rows (length level-spec))
          (columns (length (first level-spec))))
    (loop for column from 0 below columns do
        (loop for row from 0 below rows do
          (let* ((icon (intern
                         (symbol-name (nth column (nth row level-spec))) :keyword))
                  (thing (case icon
                           ;; TODO use the known icons defined on each class
                           (:o (make-egg row column))
                           (:@ (make-obstacle row column))
                           (:x (make-hole row column))
                           (:< (make-chicken row column)))))
            (when thing (push thing (state world))))))
    (setf (slot-value world 'rows) rows)
    (setf (slot-value world 'columns) columns))
  (state world))

(defmethod find-thing-if ((world world) test-fn)
  "Get the thing at at a specific coördinate."
  (find-if test-fn (state world)))

(defmethod thing-at ((world world) point)
  "Get the thing at at a specific coördinate."
  (find-thing-if world (lambda (thing) (thing-at? thing point))))

(defgeneric try-move (world thing direction)
  (:documentation
    "Returns :ok, :bad, :win or :die."))

(defmethod try-move ((world world) (thing thing) direction)
  nil)

(defmethod try-move ((world world) (egg egg) direction)
  (let* ((point (point-peek (thing-point egg) direction))
          (inhabitant (thing-at world point)))
    (if (point-within? point (rows world) (columns world))
      (typecase inhabitant
        (null :ok)
        (hole :win) ; wining egg :)
        (obstacle :bad)
        (egg (error "Amazed to find a second egg. Don't know what to do."))
        (chicken (error "How many chickens are there? Too many chickens?")))
      :bad)))

(defmethod try-move ((world world) (chicken chicken) direction)
  (let* ((point (point-peek (thing-point chicken) direction))
          (inhabitant (thing-at world point)))
    (if (point-within? point (rows world) (columns world))
      (typecase inhabitant
        (null :ok)
        (hole :die) ; dead chicken =[
        (obstacle :bad)
        (egg (try-move world inhabitant direction)) ; do whatever the egg does
        (chicken (error "Startled by another chicken.")))
      :bad)))

(defmethod find-chicken ((world world))
  (find-thing-if world 'chicken?))

(defmethod next-level ((world world))
  (incf (level-number world))
  (load-level world))

(defmethod end ((world world) why)
  (case why
    (:win (next-level world))
    (:die (load-level world))))

(defgeneric move (thing direction world)
  (:documentation "Move a thing!"))

(defmethod move ((world world) (egg egg) direction)
  (let* ((point (point-peek (thing-point egg) direction))
          (result (try-move world egg direction)))
    (if (member result '(:ok :win))
      (setf (thing-point egg) point))
    result))

(defmethod move ((world world) (chicken chicken) direction)
  (let* ((point (point-peek (thing-point chicken) direction))
          (inhabitant (thing-at world point))
          (result (try-move world chicken direction)))
    (case result
      (:ok
        (when (egg? inhabitant) (move world inhabitant direction))
        (setf (thing-point chicken) point))
      (:win (end world :win))
      (:die (end world :die)))
    result))

(defmethod move-chicken ((world world) direction)
  (format t "~a" (move world (find-chicken world) direction)))

(defun make-world (levels)
  (let ((world (make-instance 'world :levels levels)))
    (load-level world)
    world))

(defun read-levels ()
  (with-open-file
    (in (asdf:system-relative-pathname :chicken-egg-hole "levels.l"))
    (read in)))

;; better not unless i can pretty print
;; (defun write-levels (levels)
;;   (with-open-file (out "levels.l")
;;     (print levels out)))
