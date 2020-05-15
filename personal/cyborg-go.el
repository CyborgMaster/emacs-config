;; Go Mode
;;
;; We aren't using the standard prelude go module because it's based on old
;; command line tools.  A more modern environment is now based on the go
;; language server "gopls".
;;
;; Mostly taken from
;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md. With pieces
;; taken from /modules/prelude-go.el

(use-package lsp-mode
             :ensure t
             :commands (lsp lsp-deferred)
             :hook ((go-mode . lsp-deferred)
                    (lsp-mode . lsp-enable-which-key-integration)))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(defun my-go-customize-hook ()
  ;; Add to default go-mode key bindings
  (let ((map go-mode-map))
    (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
    (define-key map (kbd "C-c m") 'go-test-current-file)
    (define-key map (kbd "C-c .") 'go-test-current-test)
    (define-key map (kbd "C-c b") 'go-run)
    (define-key map (kbd "C-h f") 'godoc-at-point))

  ;; stop whitespace being highlighted
  (whitespace-toggle-options '(tabs))

  ;; CamelCase aware editing operations
  (subword-mode +1))
(add-hook 'go-mode-hook 'my-go-customize-hook)

;; The normal gotest package has a bug with testify.  PR here:
;; https://github.com/nlamirault/gotest.el/pull/70.  I forked it to rebase his
;; PR on master.
(use-package gotest
  :quelpa (gotest :fetcher github
                   :repo "CyborgMaster/gotest.el"
                   :branch "testify-rebased"))

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
             :ensure t
             :config
             ;; Optionally enable completion-as-you-type behavior.
             (setq company-idle-delay 0.5)
             (setq company-dabbrev-downcase 0)
             (setq company-minimum-prefix-length 1))

;; Optional - provides snippet support.
(use-package yasnippet
             :ensure t
             :commands yas-minor-mode
             :hook (go-mode . yas-minor-mode))
