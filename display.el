;;; display.el -*- lexical-binding: t; -*-
;;; NOTE: braille chars are 2x3

(load "~/.config/doom/util")

(setq grapher-global-frame-width 128)
(setq grapher-global-frame-height 45)
(setq grapher-global-framebuf-width (* 2 grapher-global-frame-width))
(setq grapher-global-framebuf-height (* 3 grapher-global-frame-height))
;; Create global framebuffer
(setq grapher-global-framebuf (make-vector grapher-global-framebuf-height nil))
(loopn i grapher-global-framebuf-height
       (aset grapher-global-framebuf i (make-vector grapher-global-framebuf-width nil)))

;; Framebuffer primitives
(defun grapher-prepare-screen ()
  "Clear the buffer and draw enough spaces"
  (let ((blankl (make-string grapher-x-size ? )))
    (loopn _ (/ grapher-global-framebuffer-width 2)
           (insert blankl)
           (insert (c2s ?\n)))))

(defun grapher-global-framebuf-on (pos)
  "Turn on a pixel"
  (let ((x (car pos))
        (y (cdr pos)))
    (aset (aref grapher-global-framebuffer y) x t)))

(defun grapher-global-framebuf-off (pos)
  "Turn off a pixel"
  (let ((x (car pos))
        (y (cdr pos)))
    (aset (aref grapher-global-framebuf y) x nil)))

(defun grapher--framebuf-condense-cell (pos)
  "Condense a group of framebuffer cells into a 2x3 cell at pos."
  (let ((fb-x (* 2 (car pos)))
        (fb-y (* 3 (cdr pos)))
        (braille '(?\  ?⠁ ?⠃ ?⠉ ?⠙ ?⠑ ?⠋ ?⠛ ?⠓ ?⠊ ?⠚ ?⠈ ?⠘
                   ?⠄ ?⠅ ?⠇ ?⠍ ?⠝ ?⠕ ?⠏ ?⠟ ?⠗ ?⠎ ?⠞ ?⠌ ?⠜
                   ?⠤ ?⠥ ?⠧ ?⠭ ?⠽ ?⠵ ?⠯ ?⠿ ?⠷ ?⠮ ?⠾ ?⠬ ?⠼
                   ?⠠ ?⠡ ?⠣ ?⠩ ?⠹ ?⠱ ?⠫ ?⠻ ?⠳ ?⠪ ?⠺ ?⠨ ?⠸
                   ?⠂ ?⠆ ?⠒ ?⠲ ?⠢ ?⠖ ?⠶ ?⠦ ?⠔ ?⠴ ?⠐ ?⠰)))
    (let* ((fb grapher-global-framebuf) ; lol
           (patt (list (aref-2d fb (+ 0 fb-y) (+ 0 fb-x)) ; hardcoded teehee
                       (aref-2d fb (+ 0 fb-y) (+ 1 fb-x))
                       (aref-2d fb (+ 1 fb-y) (+ 0 fb-x))
                       (aref-2d fb (+ 1 fb-y) (+ 1 fb-x))
                       (aref-2d fb (+ 2 fb-y) (+ 0 fb-x))
                       (aref-2d fb (+ 2 fb-y) (+ 1 fb-x)))))
      ;; Here they come...
      (cl-position patt
                   '((nil nil nil nil niL nil) ; line 1
                     (t nil nil nil nil nil)
                     (t nil t nil nil nil)
                     (t t nil nil nil nil)
                     (t nil nil t nil nil)
                     (t t t nil nil nil)
                     (t t t t nil nil)
                     (t nil t t nil nil)
                     (nil t t nil nil nil)
                     (nil t t t nil nil)
                     (nil t nil nil nil nil)
                     (nil t nil t nil nil)
                     (nil nil nil nil t nil) ; line 2
                     (t nil nil nil t nil)
                     (t nil t nil t nil)
                     (t t nil nil t nil)
                     (t t nil t t nil)
                     (t nil nil t t nil)
                     (t t t nil t nil)
                     (t t t t t nil)
                     (t nil t t t nil)
                     (nil t t nil t nil)
                     (nil t t t t nil)
                     (nil t nil nil t nil)
                     (nil t nil t t nil)
                     (nil nil nil nil t t) ; line 3
                     (t nil nil nil t t)
                     (t nil t nil t t)
                     (t t nil nil t t)
                     (
