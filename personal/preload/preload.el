;; Use the command key as meta and the option key as super
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Load our custom theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq prelude-theme 'ally)
