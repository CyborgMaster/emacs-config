;; Get around an https bug in Emacs 26.2 (see: https://www.reddit.com/r/emacs/
;; comments/cdei4p/failed_to_download_gnu_archive_bad_request/)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Start the emacs server so clients can connect to it through the terminal
(server-start)

;; Save and restore open buffers.
(require 'desktop)
(desktop-save-mode 1)

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

;; Add a command to revert all open buffers
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

;; Fix smooth scrolling on OSX
(setq mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control) . 5)))
(setq mouse-wheel-progressive-speed 'nil)

;; Set indent sizes for all our languages to 2 spaces
(setq standard-indent 2)
(setq-default tab-width 2)
(setq tab-width 2)

(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-deep-indent-paren nil)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq ruby-deep-arglist nil)
(setq ruby-deep-indent-paren nil)
(setq ruby-deep-indent-paren-style nil)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq c-basic-offset 2)
(add-hook 'prelude-c-mode-common-hook (lambda () (setq c-basic-offset 2)) t)
(add-hook 'prelude-c-mode-common-hook (lambda () (c-set-offset 'arglist-intro '+)))
(add-hook 'prelude-c-mode-common-hook (lambda () (c-set-offset 'arglist-cont-nonempty '+)))

;; Open up .h files in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; scss-mode seems to have broken syntax highlighting right now (See:
;; https://github.com/antonj/scss-mode/issues/36).  Normal css-mode handles 90%
;; of what we need.
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))

(setq fill-column 80) ;; Out standard wrapping column is column 80

;; Turn off auto save, gets really annoying with guard auto testing
(setq prelude-auto-save nil)
(super-save-mode 0)

;; Set the default coffeelint configuration file to what we use in our projects
(setq flycheck-coffeelintrc "coffeelint.json")

;; Default to Monaco font (can be overridden using customize-face 'default')
(set-face-attribute 'default nil :weight 'normal :height 100 :width 'normal
                    :foundry "apple" :family "Monaco")
(set-face-attribute 'fixed-pitch nil :weight 'normal :height 100 :width 'normal
                    :foundry "apple" :family "Monaco")
(setq mac-allow-anti-aliasing nil)

;; Turn of smartparens mode.  It looks like it could be very powerful if you got
;; to know it, but it really raises the learning curve.  Feel free to turn it
;; back on in your config if you are feeling ambitious.
(add-hook 'prelude-prog-mode-hook (lambda () (smartparens-mode -1)) t)

;; Javascript has a TON of different indentation patterns (the author of
;; js2-mode talks about it here:
;; http://steve-yegge.blogspot.co.nz/2008_03_01_archive.html), so js2-mode has
;; an option to bounce between different likely spots every time you push tab,
;; so we turn that on.  However, the default behavior for electric indent is to
;; reindent the previous line every time you hit `enter`, which causes js2-mode
;; to go back to it's "most-likely" guess even if you pushed tab a couple times
;; to customize it, so we have to turn off the "indent previous line" behavior
;; of electric indent.
;;
;; We turn on the bounce indent feature described above by setting the
;; `js2-bounce-indent` variable, but since `js2-mode` reads that variable at
;; load time we have to set it in `preload.el`
(add-hook 'js2-mode-hook (lambda () (setq electric-indent-inhibit t)) t)
;; ES6 allows trailing commas and it makes arrays and objects much more readable
(setq js2-strict-trailing-comma-warning nil)

;; RET is not a physical (keyboard) key. It is a logical key -- the Emacs
;; way of writing what your physical Return or Enter key typically sends to
;; Emacs: a Control-M character (which Emacs also writes as C-m, when describing
;; the Ctrl + m key sequence). And if your physical Return or Enter key sends
;; something else, then Emacs writes that as the pseudo-function key
;; <return>.
;;
;; Emacs automatically maps <return> to RET, unless <return> is itself bound to
;; a different command than RET. (Note that the ASCII control character
;; Control-M is also called a carriage return character, and C-m is essentially
;; another way of writing RET.)
;; (From: https://emacs.stackexchange.com/a/14944)
;;
;; It also seems the Emacs automatically translates M-return to M-RET, but not
;; C-return to C-RET, so we are adding that translation. See
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Translation-Keymaps.html
;; for info on how key translation works.
(define-key local-function-key-map [C-return] (kbd "C-RET"))

;; A few custom keyboard shortcuts
(global-set-key [M-s-down]  (lambda () (interactive) (scroll-up 1)))
(global-set-key [M-s-up]    (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "C-s-l") 'global-linum-mode)
(global-set-key (kbd "M-RET") (lambda () (interactive)
                                  (set-buffer-modified-p t) (save-buffer 0)))
(global-set-key (kbd "C-RET") 'pop-to-mark-command)

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

(add-hook 'prelude-prog-mode-hook (lambda () (abbrev-mode -1)))
