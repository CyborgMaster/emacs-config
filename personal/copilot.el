;; copilot
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el"))
  :ensure t
  :bind (:map copilot-mode-map
              ("<tab>" . my/copilot-tab)
              ;; ("s-n" . copilot-next-completion)
              ;; ("s-p" . copilot-previous-completion)
              ;; ("s-w" . copilot-accept-completion-by-word)
              ;; ("s-l" . copilot-accept-completion-by-line)
        )
  :init
  (setq copilot-indent-offset-warning-disable t)

  :config
  (defun my/copilot-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (indent-for-tab-command)))

  :hook
  (prog-mode . copilot-mode))
