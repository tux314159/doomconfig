;;; Bad graphing calculator

(load "~/.config/doom/parse-infix-expr")

;; Axes
(defvar grapher-x-size 128)
(defvar grapher-y-size 40)

(defmacro loopn (ivar n &rest body)
  `(let ((,ivar 0))
     (while (< ,ivar ,n)
       (progn
         ,@body
         (setq ,ivar (+ ,ivar 1))))))

(defun c2s (c)
  (make-string 1 c))

(defun grapher-draw-axes (xsz ysz)
  ;; Draw blank space
  (let ((blankl (make-string xsz ? )))
    (loopn _ ysz
           (insert blankl)
           (insert (c2s ?\n))))
  (goto-char 0)
  (forward-line (/ ysz 2))              ; go to middle vertically
  (insert (make-string xsz ?-))         ; draw x-axis
  (goto-char 0)
  (loopn i ysz                          ; draw y-axis
         (forward-char (/ xsz 2))       ; go to middle horizontally
         (insert (c2s ?|))
         (forward-line)))
