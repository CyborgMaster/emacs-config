;; Save and restore open buffers.
(require 'desktop)
(desktop-save-mode 1)

;; Use Revive+ to restore open frames.
;;
;; This is a very small library taken from
;; https://github.com/martialboniou/revive-plus.  It's not on MELPA
;; and so is installed inside of /vendor
(prelude-require-package 'revive)
(require 'revive+)
(setq revive-plus:all-frames t)
(revive-plus:demo)

;; The Monaco font for OSX is intended to be used without anti aliasing
(setq mac-allow-anti-aliasing nil)
