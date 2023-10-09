(defun is-digit (c)
  (and (>= c ?0) (<= c ?9)))

(defun parse-infix-expr (str)
  "Parse an infix math expression into a function."
  ;; First we parse it into AST
  (let ((operators (list ?* ?/ ?+ ?- nil)) ; in order of precedence
        (in (string-to-list str))
        (stack '())
        (out '())
        (buf '()))
    (while (not (null in))
      (let ((c (pop in)))
        (if (or (= c ?.) (is-digit c))
            (push c buf)                ; read the whole number in first
          (progn
            (when buf
              (push (string-to-number (concat (reverse buf))) out) ; push number
              (setq buf '()))
            (cond
             ((= c ?n)                  ; the argument
              (push 'n out))
             ((member c operators)      ; operators
              (while (and (member (car stack) operators)
                          (< (cl-position (car stack) operators) ; while there's an op of
                             (cl-position c operators))) ; greater precedence on the stack
                (pop-operator-and-push stack out)) ; pop it and push it to output
              (push c stack))          ; then push the current op onto the stack
             ((= c ?\()                ; left bracket
              (push c stack))
             ((= c ?\))                 ; right bracket
              (while (/= (car stack) ?\()
                (pop-operator-and-push stack out))
              (pop stack)))))))
    (if buf
        (push (string-to-number (concat (reverse buf))) out)) ; push number if any
    (while (not (null stack))
      (pop-operator-and-push stack out))
    ;; Generate the function :D
    `(lambda (n) ,(car out))))
