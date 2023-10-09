;;; Bad graphing calculator

(load "~/.config/doom/parse-infix-expr")

;; Axes
(setq grapher-x-size 127)
(setq grapher-y-size 45)
(setq grapher-x-scale 3)

(defmacro loopn (ivar n &rest body)
  "Loop n times"
  `(let ((,ivar 0))
     (while (< ,ivar ,n)
       (progn
         ,@body
         (setq ,ivar (+ ,ivar 1))))))

(defun c2s (c)
  "Convert character to a string"
  (make-string 1 c))

(defun overwrite-char (c)
  (delete-char 1)
  (insert (c2s c)))

(defun succ (n)
  (+ n 1))

(defun pred (n)
  (+ n 1))

(defun grapher-calc-pos (x y)
  "Calculate the point of coordinates"
  (let ((zero
         (+
          (* (succ grapher-x-size) (/ grapher-y-size 2))
          (+ (/ grapher-x-size 2) 2))))
    (+ zero
       (- (* (succ grapher-x-size) (round y)))
       (round x))))

(defun grapher-draw-axes ()
  "Initialise the graph axes, should be called on a blank buffer."
  ;; Draw blank space
  (let ((blankl (make-string grapher-x-size ? )))
    (loopn _ grapher-y-size
           (insert blankl)
           (insert (c2s ?\n))))
  ;; Draw x-axis
  (goto-char 0)
  (forward-line (/ grapher-y-size 2))   ; go to middle vertically
  (loopn _ grapher-x-size (overwrite-char ?-))
  ;; Draw y-axis
  (goto-char 0)
  (loopn i grapher-y-size
         (forward-char (pred (/ grapher-x-size 2)))
         (overwrite-char ?|)
         (forward-line))
  ;; Draw origin
  (goto-char (grapher-calc-pos 0 0))
  (overwrite-char ?O))

(defun grapher-plot-point (pos)
  "Plot a single point."
  (goto-char (grapher-calc-pos (car pos) (cdr pos)))
  (overwrite-char ?*))

(defun grapher-gen-interval (f)
  "Generate x and y coordinates over the whole x-range for a function."
  (let ((points '())
        (x (- (/ grapher-x-size 2))))
    (while (<= x (/ grapher-x-size 2))
      (progn
        (let ((y (funcall f (/ x grapher-x-scale))))
          (when (<= (abs y) (/ grapher-y-size 2))
            (push (cons x y) points)))
        (setq x (+ x grapher-x-scale))))
    points))

(defun grapher-plot-function (f)
  "Plots a function."
  (mapc #'grapher-plot-point (grapher-gen-interval f)))

(defun grapher-interactive-plot (expr)
  (interactive "MEnter function: ")
  (erase-buffer)
  (grapher-draw-axes)
  (grapher-plot-function (parse-infix-expr expr)))
