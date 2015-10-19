;; Start the emacs server so clients can connect to it through the terminal
(server-start)

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

;; Since we swapped super and meta (see preload.el), add back the
;; Command-` shortcut so it operates like the rest of OSX
(define-key global-map "\M-`" 'other-frame)

(scroll-bar-mode -1)

;; Allow searching with ag (Silver Searcher)
(prelude-require-package 'ag)

;; Allow arrow keys without warnings
(setq prelude-guru nil)

;; Smarter comment command
(defun comment-eclipse (&optional arg)
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (if (and (not (region-active-p)) (looking-at "[ \t]*$"))
      (comment-dwim arg)
    (when (region-active-p)
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end))))
(global-set-key "\M-;" 'comment-eclipse)

;; Fix smooth scrolling on OSX
(setq mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control) . 5)))
(setq mouse-wheel-progressive-speed 'nil)

;; Set indent sizes for all our languages to 2 spaces
(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-deep-indent-paren nil)
(setq js-indent-level 2)
(setq ruby-deep-arglist nil)
(setq ruby-deep-indent-paren nil)
(setq ruby-deep-indent-paren-style nil)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)

;; Default to Monaco font (can be overridden using customize-face 'default')
(set-face-attribute 'default nil :weight 'normal :height 100 :width 'normal
                    :foundry "apple" :family "Monaco")
(setq mac-allow-anti-aliasing nil)

;; A few custom keyboard shortcuts
(global-set-key [M-s-down]  (lambda () (interactive) (scroll-up 1)))
(global-set-key [M-s-up]    (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "C-s-l") 'global-linum-mode)
(global-set-key [M-return]      (lambda () (interactive)
                                  (set-buffer-modified-p t) (save-buffer 0)))

;; To customize your Emacs config, create a personal.el file in this
;; directory (personal/personal.el).  Anything you add to it will be
;; automatically executed on Emacs start.
;;
;; Here are a few things that people commonly add:

;; Enable CUA-plus mode (Ralph's tweaked compatibility library)
;; (load (expand-file-name "optional/cua-plus.el" prelude-personal-dir))

;; Change the font (also turns on anti-aliasing)
;; (setq mac-allow-anti-aliasing 1)
;; (set-face-attribute 'default nil :weight 'normal :height 150 :width 'normal :foundry "apple" :family "andale mono")

;; Turn on line number globally
;; (global-linum-mode)
