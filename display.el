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
        (braille '( ⠁ ⠃ ⠉ ⠙ ⠑ ⠋ ⠛ ⠓ ⠊ ⠚ ⠈ ⠘
                    ⠄ ⠅ ⠇ ⠍ ⠝ ⠕ ⠏ ⠟ ⠗ ⠎ ⠞ ⠌ ⠜
                    ⠤ ⠥ ⠧ ⠭ ⠽ ⠵ ⠯ ⠿ ⠷ ⠮ ⠾ ⠬ ⠼
                    ⠠ ⠡ ⠣ ⠩ ⠹ ⠱ ⠫ ⠻ ⠳ ⠪ ⠺ ⠨ ⠸
                    ⠀ ⠂ ⠆ ⠒ ⠲ ⠢ ⠖ ⠶ ⠦ ⠔ ⠴ ⠐ ⠰)))
