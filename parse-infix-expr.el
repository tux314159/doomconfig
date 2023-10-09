(defun is-digit (c)
  (and (>= c ?0) (<= c ?9)))

(defun position-2d (elem list)
  "Find row within 2d list. Terrible code. Only works for numbers!"
  (let (i! j!)
    (let ((i 0))
      (dolist (l list i)
        (let ((j 0))
          (dolist (x l j)
            (if (eq x elem) (setq j! j))
            (setq j (+ 1 j))))
        (if (and j! (not i!))
            (setq i! i))
        (setq i (+ i 1))))
    i!))

(defun member-2d (elem list)
  "Check if element is in 2d list. Only works for numbers!"
  (not (eq (position-2d elem list) nil)))

(defun parse-infix-expr (str)
  "Parse an infix math expression into a function."
  ;; First we parse it into AST
  (let ((operators '((?* ?/) (?+ ?-) (nil))) ; in order of precedence
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
             ((member-2d c operators)   ; operators
              (while (and (member-2d (car stack) operators)
                          (<= (position-2d (car stack) operators)
                              (position-2d c operators))) ; while there's an operator of greater-
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
