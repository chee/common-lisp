(in-package :chicken-egg-hole)

(defun clear nil
  (format t "~C[2J" #\Esc))

(defun eval-key (char world)
  (case char
    (#\w (move-chicken world :up))
    (#\s (move-chicken world :down))
    (#\a (move-chicken world :left))
    (#\d (move-chicken world :right))

    (#\A (move-chicken world :up))
    (#\B (move-chicken world :down))
    (#\D (move-chicken world :left))
    (#\C (move-chicken world :right))

    (#\eot (uiop:quit))
    (#\q (uiop:quit))))

(defun run nil
  (let ((world (make-world (read-levels))))
    ;; lol it's a read-eval-print-loop !
    (loop
      (clear)
      (print-state world :human t)
(finish-output)
      (eval-key
        (trivial-raw-io:read-char)
        world))))
