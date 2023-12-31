;;; vim-number-inc-dec.el -*- lexical-binding: t; -*-
;;; Mm sweet c-a c-x

(load "~/.config/doom/parse-infix-expr")

;;; Char tests
(defun is-digit (c)
  (and (>= c ?0) (<= c ?9)))
(defun not-nl (c)
  (/= c ?\n))

(defun skip-num-backward ()
  (skip-chars-backward "0-9")
  ;; If there's a minus-sign include it (only one!)
  (if (= (char-after (- (point) 1)) ?-)
      (backward-char)))

(defun skip-num-forward ()
  (skip-chars-forward "-0-9"))

;;; Find the start of the next number
(defun find-next-num-lit-start ()
  (while (and (not-nl (char-after)) (not (is-digit (char-after)))) ; find a number
    (forward-char))
  (skip-num-backward))                  ; don't skip the minus-sign

;;; Find the start/end of the current/next number on this line
(defun find-num-lit ()
  (let ((num-start-point 0)
        (num-end-point 0))
    ;; Find the start of the number
    (if (is-digit (char-after))
        (skip-num-backward)             ; we are in the number already
      (find-next-num-lit-start))
    (setq num-start-point (point))
    ;; Find the end of the number
    (skip-num-forward)
    (setq num-end-point (point))
    ;; Ensure we actually found a number
    (if (= num-start-point num-end-point)
        'nil
      (cons num-start-point num-end-point))))

(defun get-num-lit (pts)
  (string-to-number (buffer-substring (car pts) (cdr pts))))

;;; Find the next/current number and perform some operation on it
(defun num-lit-do-op (op)
  (setq pts (find-num-lit))
  (if pts
      (let* ((len (- (cdr pts) (car pts)))
             (old-num-str (buffer-substring (car pts) (cdr pts)))
             (new-num (number-to-string (funcall op (string-to-number old-num-str)))))
        (if (= (aref old-num-str 0) ?-)
            (setq len (- len 1))) ; if there's a - don't include it in length calculations
        (setq new-num             ; add 0-padding if needed
              (concat
               (make-string (max 0 (- len (length new-num))) ?0)
               new-num))
        ;; Delete old number and insert new one
        (delete-region (car pts) (cdr pts))
        (goto-char (car pts))
        (insert new-num)
        (backward-char))))              ; we want cursor to be on the number

(defun num-lit-global-op (n)
  n)                                    ; do nothing by default

(defmacro pop-read (stack)
  `(car (read-from-string (make-string 1 (pop ,stack)))))

(defmacro pop-operator-and-push (stack out)
  "Pop an operator from the operator stack push it as a new node."
  `(let ((op (pop-read ,stack))
         (rarg (pop ,out))
         (larg (pop ,out)))
     (push (list op larg rarg) ,out)))

(defun num-lit-global-op-set (expr)
  (interactive "MSet arith operation: ")
  (fset #'num-lit-global-op (parse-infix-expr expr)))
