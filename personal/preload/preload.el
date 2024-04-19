;; We have to set the shell before `prelude-macos` tries the load the $PATH
(setq shell-file-name "/bin/zsh")

;; Use the command key as meta and the option key as super
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Load our custom theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq prelude-theme 'ally)

;; This option has to be set before `js2-mode` loads.  (See explanation of why
;; we set it in `ally.el`)
(setq js2-bounce-indent-p t)
