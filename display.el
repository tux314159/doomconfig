;;; display.el -*- lexical-binding: t; -*-

(load "~/.config/doom/util")

(setq grapher-global-framebuffer-width 256)
(setq grapher-global-framebuffer-height 90)
;; Create global framebuffer
(setq grapher-global-framebuffer (make-vector grapher-global-framebuffer-height nil))
(loopn i grapher-global-framebuffer-height
       (aset (aref grapher-global-framebuffer n))
(setq grapher-global-framebuffer (make-vector grapher-global-framebuffer-height nil))
