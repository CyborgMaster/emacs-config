;; Copied from /modules/prelude-ruby.el
;; We replace ruby-mode with enh-ruby-mode, which is by all accounts better

(require 'prelude-programming)

(prelude-require-packages '(enh-ruby-mode ruby-tools inf-ruby yari))

(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))

;; Rake files are ruby, too, as are gemspecs, rackup files, and gemfiles.
(add-to-list 'auto-mode-alist '("\\.rake\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.cap\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Podfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.podspec\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Puppetfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Appraisals\\'" . enh-ruby-mode))

(add-to-list 'auto-mode-alist '("\\.prawn\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\Bowerfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.shaper\\'" . enh-ruby-mode))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(define-key 'help-command (kbd "R") 'yari)

(eval-after-load 'enh-ruby-mode
  '(progn
     (defun prelude-ruby-mode-defaults ()
       (inf-ruby-minor-mode +1)
       (ruby-tools-mode +1)
       ;; CamelCase aware editing operations
       (subword-mode +1))

     (setq prelude-ruby-mode-hook 'prelude-ruby-mode-defaults)

     (add-hook 'enh-ruby-mode-hook (lambda ()
                                 (run-hooks 'prelude-ruby-mode-hook)))))
