(prelude-require-package 'revive)

(require 'revive+)
(setq revive-plus:all-frames t)
(revive-plus:demo)

(require 'prelude-helm-everywhere)

;; The Monaco font for OSX is built without anti aliasing
(setq mac-allow-anti-aliasing nil)
