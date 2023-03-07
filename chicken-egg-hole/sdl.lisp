(in-package :chicken-egg-hole)

(defun load-image (thing)
  (sdl2-image:load-image
    (asdf:system-relative-pathname
      :chicken-egg-hole (format nil "img/~A.png" thing))))

(defvar *images* nil)
(defun load-images ()
  (dolist (thing '(chicken egg hole obstacle))
    (setf (getf *images* thing) (load-image thing))))

(defvar *thing-size* 100)

(defun thing-rect (thing)
  (sdl2:make-rect
    (* (thing-column thing) *thing-size*)
    (* (thing-row thing) *thing-size*)
    *thing-size*
    *thing-size*))

(defun start-sdl nil
  (sdl2-image:init (list :png))
  (load-images)
  (let ((world (make-world (read-levels))))
    (sdl2:with-init (:everything)
      (sdl2:with-window (window
                          :title "Chicken -> Egg -> Hole"
                          :w (* *thing-size* (columns world))
                          :h (* *thing-size* (rows world))
                          :flags '(:shown))
        (let ((surface (sdl2:get-window-surface window)))
          (sdl2:with-event-loop (:method :poll)
            (:quit () t)
            (:keydown (:keysym keysym)
              (let ((scancode (sdl2:scancode-value keysym)))
                (cond
                  ((or
                     (sdl2:scancode= scancode :scancode-up)
                     (sdl2:scancode= scancode :scancode-w))
                    (move-chicken world :up))
                  ((or
                     (sdl2:scancode= scancode :scancode-down)
                     (sdl2:scancode= scancode :scancode-s))
                    (move-chicken world :down))
                  ((or
                     (sdl2:scancode= scancode :scancode-left)
                     (sdl2:scancode= scancode :scancode-a))
                    (move-chicken world :left))
                  ((or
                     (sdl2:scancode= scancode :scancode-right)
                     (sdl2:scancode= scancode :scancode-d))
                    (move-chicken world :right))
                  ((sdl2:scancode= scancode :scancode-q) (uiop:quit)))))

            (:idle ()
              (sdl2:fill-rect surface nil #xfffff)
              (dolist (thing (state world))
                (sdl2:blit-scaled
                  (getf *images* (type-of thing))
                  nil
                  surface
                  ;; maybe shouldn't be making all these rectangles
                  (thing-rect thing)))
              (sdl2:update-window window)
              (sdl2:delay 10))))))))

#+os-macosx
(defun run nil (sdl2:make-this-thread-main #'start-sdl))
#-os-macosx
(defun run nil (start-sdl))
