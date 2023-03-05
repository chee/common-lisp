(defpackage egghole (:use :cl))
(in-package :egghole)

(require :alexandria)
(require :sdl2)
(require :sdl2-image)
(sdl2-image:init (list :png))


(defmacro -> (fn &rest args)
  (let ((rest (gensym "REST-")))
    `(lambda (&rest ,rest)
       (apply #',fn ,@args ,rest))))

(defun path (path)
  (asdf:system-relative-pathname :egg path))

(defun img-path (name)
  (path (format nil "img/~A.png" name)))

(defun load-img (name)
  (sdl2-image:load-image (img-path name)))

(defvar *chicken-img* (load-img :chicken))
(defvar *egg-img* (load-img :egg))
(defvar *hole-img* (load-img :hole))
(defvar *obstacle* (load-img :obstacle))
(defvar *window-width* 800)
(defvar *window-height* 600)
(defvar *thing-size* 100)
(defun rows ()
  (/ *window-height* *thing-size*))
(defun columns ()
  (/ *window-width* *thing-size*))

(defstruct thing name icon image row column)

(defun create-chicken (&optional (row 0) (column 0))
  (make-thing
    :name :chicken
    :icon "<"
    :image (load-img :chicken)
    :row row
    :column column))

(defun create-egg (&optional (row 0) (column 0))
  (make-thing
    :name :egg
    :icon "<"
    :image (load-img :egg)
    :row row
    :column column))

(defun create-hole (&optional (row 0) (column 0))
  (make-thing
    :name :hole
    :icon "<"
    :image (load-img :hole)
    :row row
    :column column))

(defun create-obstacle (&optional (row 0) (column 0))
  (make-thing
    :name :obstacle
    :icon "<"
    :image (load-img :obstacle)
    :row row
    :column column))

(defun thing-rect (thing)
  (sdl2:make-rect
    (* (thing-column thing) *thing-size*)
    (* (thing-row thing) *thing-size*)
    *thing-size*
    *thing-size*))

(defvar level-1
  (make-array '(6 8)
    :initial-contents
    '(
       (_ _ _ _ _ _ _ _)
       (_ _ _ _ o _ _ _)
       (_ _ _ @ _ _ _ _)
       (_ _ _ @ X _ _ _)
       (_ _ < _ _ _ _ _)
       (_ _ _ _ _ _ _ _ ))))

(defvar level-2
  (make-array '(6 8)
    :initial-contents
    '(
       (< _ @ _ _ _ _ _)
       (_ _ _ _ _ @ _ _)
       (_ _ _ @ _ _ _ _)
       (_ _ _ o _ _ _ _)
       (_ _ _ _ _ _ _ _)
       (_ _ X _ _ _ _ _))))

(defvar level-3
  (make-array '(6 8)
    :initial-contents
    '(
       (_ _ _ _ _ _ _ _)
       (_ _ _ _ o _ _ _)
       (_ @ _ @ _ @ _ _)
       (_ _ _ X _ _ _ _)
       (_ _ < _ _ _ _ _)
       (_ _ _ _ _ _ _ _))))

(defun random-level ()
  (let ((level (make-array (list (rows) (columns)) :initial-element '_)))
    (let ((chicken-column (random (columns)))
          (chicken-row (random (rows))))
      (setf (aref level chicken-row chicken-column) '<)
      (let ((egg-column (random (columns)))
            (egg-row (random (rows))))
        (loop until (and (/= egg-row chicken-row) (/= egg-column chicken-column))
          do (setf egg-row (random (rows))
                   egg-column (random (columns))))
        (setf (aref level egg-row egg-column) 'o)
        (let ((hole-column (random (columns)))
              (hole-row (random (rows))))
          (loop until (and (/= hole-row chicken-row) (/= hole-column chicken-column) (/= hole-row egg-row) (/= hole-column egg-column))
            do (setf hole-row (random (rows))
                     hole-column (random (columns))))
          (setf (aref level hole-row hole-column) 'X))))
    level))

(defun find-path (start end world)
  (let ((grid (make-array '(6 8) :initial-element :empty)))
    (dolist (obstacle (obstacles world))
      (destructuring-bind (x y) (position obstacle world)
        (setf (aref grid x y) :obstacle)))

    (dolist (obj (holes world))
      (destructuring-bind (x y) (position obj world)
        (setf (aref grid x y) :passable)))

    (dolist (obj (eggs world))
      (destructuring-bind (x y) (position obj world)
        (setf (aref grid x y) :passable)))

    (destructuring-bind (x0 y0) start
      (setf (aref grid x0 y0) :start))

    (destructuring-bind (x1 y1) end
      (setf (aref grid x1 y1) :end))

    (let ((open (list start))
          (closed (make-hash-table))
          (came-from (make-hash-table))
          (g-scores (make-hash-table))
          (f-scores (make-hash-table)))
      (setf (gethash start g-scores) 0)
      (setf (gethash start f-scores) (heuristic-cost-estimate start end))
      (loop while open do
           (let* ((current (pop open))
                  (current-g (gethash current g-scores))
                  (neighbors (find-passable-neighbors current grid)))
             (loop for neighbor in neighbors do
                  (let ((tentative-g (+ current-g (movement-cost current neighbor))))
                    (unless (gethash neighbor g-scores)
                      (push neighbor open)
                      (setf (gethash neighbor f-scores)
                        (+ tentative-g (heuristic-cost-estimate neighbor end))))
                    (when (or (< tentative-g (gethash neighbor g-scores))
                              (not (gethash neighbor g-scores)))
                      (setf (gethash neighbor g-scores) tentative-g)
                      (setf (gethash neighbor f-scores)
                        (+ tentative-g (heuristic-cost-estimate neighbor end)))
                      (setf (gethash neighbor came-from) current))))
             (when (equal current end)
               (return (reconstruct-path came-from end))))))))

;;(find-path '(4 2) '(3 4) level-1)

(defun level-valid-p (level)
  (equal (array-dimensions level) '(6 8)))

(defun create-world (level-spec)
  (destructuring-bind (rows columns) (array-dimensions level-spec)
    (let (level)
      (loop for column from 0 below columns do
        (loop for row from 0 below rows do
          (let* ((icon (aref level-spec row column))
                  (thing (case icon
                      (o (create-egg row column))
                      (@ (create-obstacle row column))
                      (X (create-hole row column))
                      (< (create-chicken row column)))))
            (when thing (push thing level)))))
      level)))
(defvar *level* 0)
(defvar *levels* (list level-1 level-2 level-3))
(defvar *world* (create-world level-1))

(defun thing-is? (thing name)
  (equal (thing-name thing) name))

(defun chicken? (thing)
  (thing-is? thing :chicken))

(chicken? (create-chicken))
(defun egg? (thing)
  (thing-is? thing :egg))
(defun hole? (thing)
  (thing-is? thing :hole))
(defun obstacle? (thing)
  (thing-is? thing :obstacle))

(defun thing-at (world row column)
   "Get the thing at at a specific coördinate."
  (find-if
    (lambda (thing)
      (and
        (= (thing-row thing) row)
        (= (thing-column thing) column)))
    world))

(defun peek (thing direction)
  (let* ((column (thing-column thing))
          (row (thing-row thing))
          (next-column
            (case direction
              ((:up :down) column)
              (:left (when (> column 0) (- column 1)))
              (:right (when (< column (- (columns) 1)) (+ column 1)))))
          (next-row
            (case direction
              ((:left :right) row)
              (:up (when (> row 0) (- row 1)))
              (:down (when (< row (- (rows) 1)) (+ row 1))))))
    (when (and next-row next-column)
      (list next-row next-column))))


(defun intersect? (thing-1 thing-2)
  (unless (eql thing-1 thing-2)
    (when (sdl2:intersect-rect
            (thing-rect thing-1)
            (thing-rect thing-2))
      thing-2)))

(defun move-thing (thing direction world)
  (let ((coördinate (peek thing direction)))
    (when coördinate
      (setf (thing-row thing) (first coördinate))
      (setf (thing-column thing) (second coördinate)))
    coördinate))

(defun opposite (direction)
  (case direction
    (:up :down)
    (:left :right)
    (:down :up)
    (:right :left)))

(defun end (game why)
  (case why
    ('die (format t "chicken hole :(")
      (setf *world* (create-world (or (handler-case (elt *levels* *level*)
                            (error (random-level)))
                        (random-level)))))
    ('win (format t "chicken egg hole :)")
      (setf *world* (create-world
                      (or (handler-case (elt *levels* (incf *level*))
                            (error (random-level)))
                        (random-level)))))))

(defmacro funcase (keyform &rest clauses)
  `(let ((key ,keyform))
     (cond ,@(mapcar (lambda (clause)
                       (destructuring-bind (pred form) clause
                         `((funcall ',pred key) ,form)))
               clauses))))

;; TODO let things decide how to handle being moved into by a chicken
(defun move-chicken (direction world game)
  (let ((chicken (find-if #'chicken? world)))
    (move-thing chicken direction world)
    (let ((collision (find-if (-> intersect? chicken) world)))
      (when collision
        (cond
          ((hole? collision) (end game 'die))
          ((egg? collision) (unless (move-egg direction world game)
                              (print "egg bad in chicken")
                              (move-thing chicken (opposite direction) world)))
          ((obstacle? collision) (move-chicken (opposite direction) world game)))))))

(defun move-egg (direction world game)
  (let* ((egg (find-if #'egg? world))
          (moved
            (move-thing egg direction world)))
    (when moved
      (let ((collision (find-if (-> intersect? egg) world)))
        (if collision
          (cond
            ((hole? collision) (end game 'win))
            ((obstacle? collision)
              (print "egg bad in egg")
              (move-thing egg (opposite direction) world)
              nil))
          t)))))

(defun run nil
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :flags '(:shown))
      (let ((surface (sdl2:get-window-surface window)))
        (sdl2:with-event-loop (:method :poll)
          (:quit () t)
          (:keydown (:keysym keysym)
            (let ((scancode (sdl2:scancode-value keysym))
                   (sym (sdl2:sym-value keysym))
                   (mod-value (sdl2:mod-value keysym)))
              (cond
                ((or
                   (sdl2:scancode= scancode :scancode-up)
                   (sdl2:scancode= scancode :scancode-w))
                  (move-chicken :up *world* nil))
                ((or
                   (sdl2:scancode= scancode :scancode-down)
                   (sdl2:scancode= scancode :scancode-s))
                  (move-chicken :down *world* nil))
                ((or
                   (sdl2:scancode= scancode :scancode-left)
                   (sdl2:scancode= scancode :scancode-a))
                  (move-chicken :left *world* nil))
                ((or
                   (sdl2:scancode= scancode :scancode-right)
                   (sdl2:scancode= scancode :scancode-d))
                  (move-chicken :right *world* nil))
                ((sdl2:scancode= scancode :scancode-q) (exit)))))

          (:idle ()
            (sdl2:fill-rect surface nil #xfff)
            (dolist (thing *world*)
              (sdl2:blit-scaled
                (thing-image thing)
                nil
                surface
                (thing-rect thing)))
            (sdl2:update-window window)
            (sdl2:delay 10)))))))

(defun start nil (sdl2:make-this-thread-main #'run))
