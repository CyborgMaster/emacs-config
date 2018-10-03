(prelude-require-package 'mmm-mode)

(require 'mmm-auto)

(setq mmm-global-mode 'auto)

(mmm-add-group
 'html-js-no-templates
 '((js-script-cdata-no-templates
    :submode js-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*\\(//\\)?]]>[ \t\n]*</script>")
   (js-script-no-templates
    :submode js-mode
    :face mmm-code-submode-face
    :front "<script\\([^t]\\|t[^e]\\|te[^m]\\|tem[^p]\\|temp[^l]\\|templ[^a]\\|templa[^t]\\|templat[^e]\\)*?>[ \t]*\n?"
    :back "[ \t]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">\n"
                 @ "" _ "" @ "\n</script>" @)))))

(mmm-add-mode-ext-class 'html-erb-mode "\\.html.*\\.erb\\'" 'erb)
(mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)
(mmm-add-mode-ext-class 'html-erb-mode nil 'html-js-no-templates)
(mmm-add-mode-ext-class 'html-erb-mode nil 'html-css)

(mmm-add-mode-ext-class 'html-mode nil 'html-js)
(mmm-add-mode-ext-class 'html-mode nil 'html-css)

(add-to-list 'auto-mode-alist '("\\.html.*\\.erb\\'" . html-erb-mode))
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))

(setq mmm-submode-decoration-level 1)
(setq mmm-parse-when-idle t)
(global-set-key "\M-p"  'mmm-parse-buffer)

(add-hook 'mmm-ruby-mode-submode-hook
          (lambda ()
            (whitespace-mode 0)
            (hl-todo-mode)))
(add-hook 'mmm-js-mode-submode-hook
          (lambda ()
            (whitespace-mode 0)))
