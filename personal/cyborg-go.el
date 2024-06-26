;; Go Mode
;;
;; We aren't using the standard prelude go module because it's based on old
;; command line tools.  A more modern environment is now based on the go
;; language server "gopls".
;;
;; Mostly taken from
;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md. With pieces
;; taken from /modules/prelude-go.el

(prelude-require-packages '(go-mode
                            go-projectile
                            lsp-mode
                            lsp-ui
                            company
                            gotest))

(use-package lsp-mode
             :ensure t
             :commands (lsp lsp-deferred)
             :config
             ;; Use projectile to detect the project root
             (setq lsp-auto-guess-root t)
             :hook (
                    (go-mode . lsp-deferred)
                    (lsp-mode . lsp-enable-which-key-integration)
                    ))

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

(use-package gotest
  :ensure t
  :config (setq-default go-test-args "-race"))

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  ;; The pop up docs are currently slowing down movement. So we disable it for
  ;; now, but decrease the delay when we invoke it manually.
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-delay 0)
  :config
  ;; lsp-ui remaps find-definitions to lsp-ui-peek-find-definitions, which pops
  ;; up in the current window.  It pops up two things side by side, which with
  ;; my narrower windows, don't have enough width to read.  I revert to using
  ;; the default which jumps to the file.
  (define-key lsp-ui-mode-map [remap xref-find-definitions] nil)
)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol
  :init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

;; Company mode is a standard completion package that works well with lsp-mode.
;; (use-package company
;;   :ensure t
;;   :config
;;   ;; Optionally enable completion-as-you-type behavior.
;;   (setq company-idle-delay 0.5)
;;   (setq company-dabbrev-downcase 0)
;;   (setq company-minimum-prefix-length 1))

;; Provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

;; Auto gen stubs for missing functions
(use-package go-impl
  :quelpa (go-impl
           :fetcher github
           :repo "krizex/emacs-go-impl"
           :branch "godoc"))
