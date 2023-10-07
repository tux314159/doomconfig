;;; vim-number-inc-dec.el -*- lexical-binding: t; -*-
;;; Mm sweet c-a c-x

(defun is-digit (c)
  (and (>= c ?0) (<= c ?9)))
(defun not-nl (c)
  (/= c ?\n))

(defun find-num-lit ()
  ;; Find the start of the number
  (if (is-digit (char-after))
      (skip-chars-backward "0-9") ; we are in the number already
    (while (and (not-nl (char-after)) (not (is-digit (char-after)))) ; find a number
      (forward-char)))
  (setq num-start-point (point))
  ;; Find the end of the number
  (skip-chars-forward "0-9")
  (setq num-end-point (point))
  ;; Ensure we actually found a number
  (if (= num-start-point num-end-point)
      'nil
    (cons num-start-point num-end-point)))

(defun get-num-lit (pts)
  (string-to-number (buffer-substring (car pts) (cdr pts))))

(defun inc-num-lit ()
  (setq pts (find-num-lit))
  (if pts
      (let ((len (- (cdr pts) (car pts)))
            (new-num (number-to-string (+ 1 (get-num-lit pts)))))
        (setq new-num ; add 0-padding if needed
              (concat
               (make-string (max 0 (- len (length new-num))) ?0)
               new-num))
        ;; Delete old number and insert new one
        (delete-region (car pts) (cdr pts))
        (goto-char (car pts))
        (insert new-num)
        (backward-char)))) ; we want cursor to be on the number
