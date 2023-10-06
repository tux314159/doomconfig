;;; vim-number-inc-dec.el -*- lexical-binding: t; -*-

;;; Mm sweet c-a c-x
(defun is-digit (c)
  (and (>= c 48) (<= c 57)))

(defun find-num-lit ()
  ; find the start of the number
  (if (is-digit (following-char))  ; we are in a number already fdfsd 43243 f
      (progn
        (while (is-digit (following-char)) (backward-char))
        (forward-char))  ; else we end up one before
    (while (not (is-digit (following-char))) (forward-char)))
  (setq num-start-point (point))
  ; find the end of the number
  (while (is-digit (following-char)) (forward-char))
  (setq num-end-point (point))
  (cons num-start-point num-end-point))

(defun get-num-lit (pts)
  (string-to-number (buffer-substring (car pts) (cdr pts))))

(defun inc-num-lit ()
  (let* ((pts (find-num-lit))
         (len (- (cdr pts) (car pts)))
         (new-num (number-to-string (+ 1 (get-num-lit pts)))))
    (setq new-num  ; add 0-padding if needed
          (concat
           (make-string (max 0 (- len (length new-num))) ?0)
           new-num))
    ; delete old number and insert new one
    (delete-region (car pts) (cdr pts))
    (goto-char (car pts))
    (insert new-num)
    (backward-char)))  ; we want cursor to be on the number
