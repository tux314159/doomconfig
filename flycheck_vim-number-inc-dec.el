;;; vim-number-inc-dec.el -*- lexical-binding: t; -*-
;;; Mm sweet c-a c-x

(defun is-digit (c)
  (and (>= c 48) (<= c 57)))
(defun not-nl (c)
  (not (= c ?\n)))

(defun find-num-lit ()
  ;; Find the start of the number
  (if (is-digit (char-after))       ; we are in a number already
      (progn
        (while (and (not-nl (char-after)) (is-digit (char-after))
          (backward-char))
        (forward-char))                 ; else we end up one before
    (while (and (not (= (char-after) ?\n)) (not (is-digit c)))
      (forward-char)))
  (setq num-start-point (point))
  ;; Find the end of the number
  (while (is-digit (char-after)) (forward-char))
  (setq num-end-point (point))
  (cons num-start-point num-end-point))

(defun get-num-lit (pts)
  (string-to-number (buffer-substring (car pts) (cdr pts))))

(defun inc-num-lit ()
  (let* ((pts (find-num-lit))
         (len (- (cdr pts) (car pts)))
         (new-num (number-to-string (+ 1 (get-num-lit pts)))))
    (setq new-num                       ; add 0-padding if needed
          (concat
           (make-string (max 0 (- len (length new-num))) ?0)
           new-num))
    ;; Delete old number and insert new one
    (delete-region (car pts) (cdr pts))
    (goto-char (car pts))
    (insert new-num)
    (backward-char)))                   ; we want cursor to be on the number
