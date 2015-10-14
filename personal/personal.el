;; Use Revive+ to restore open frames and buffers.
;;
;; This is a very small library taken from
;; https://github.com/martialboniou/revive-plus.  It's not on MELPA
;; and so is installed inside of /vendor
(prelude-require-package 'revive)
(require 'revive+)
(setq revive-plus:all-frames t)
(revive-plus:demo)

;; Turn on helm for awesome auto completion
(require 'prelude-helm-everywhere)

;; The Monaco font for OSX is intended to be used without anti aliasing
(setq mac-allow-anti-aliasing nil)
