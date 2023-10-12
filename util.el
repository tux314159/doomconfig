;;; util.el -*- lexical-binding: t; -*-

(defmacro loopn (ivar n &rest body)
  "Loop n times"
  `(let ((,ivar 0))
     (while (< ,ivar ,n)
       (progn
         ,@body
         (setq ,ivar (+ ,ivar 1))))))

(defmacro for (start stop step &rest body)
  "Loop n times"
  `(let (,start)
     (while (not ,stop)
       (progn
         ,@body
         (setq ,(car start) ,step)))))

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

(defun aref-2d (arr i j)
  (aref (aref arr i) j))
