(deftheme ally
  "Created by Ralph Mickelson (AllyDVM) 2014-05-29.")

(custom-theme-set-faces
 'ally
 '(default ((t (:foreground "#eeeeee" :background "#101418" :inherit nil))))
 '(cursor ((t (:foreground "black" :background "#ffffff")))) '(cursor ((t (:foreground "black" :background "#ffffff"))))
 '(fixed-pitch ((t (:family "fixed"))))
 '(variable-pitch ((t (:family "helv"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "medium blue"))))
 '(highlight ((t (:background "#2A2E32"))))
 '(region ((t (:background "#3d4c5c"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((t (:foreground "gray10" :background "yellow"))))
 '(trailing-whitespace ((t (:background "#102e4e"))))
 '(font-lock-builtin-face ((t (:foreground "LightCoral"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#dd4466"))))
 '(font-lock-constant-face ((t (:foreground "#6060ff"))))
 '(font-lock-doc-face ((t (:foreground "LightGreen"))))
 '(font-lock-function-name-face ((t (:foreground "#dad800"))))
 '(font-lock-keyword-face ((t (:foreground "DeepSkyBlue1"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "gold"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#ee6622"))))
 '(font-lock-type-face ((t (:foreground "CadetBlue1"))))
 '(font-lock-variable-name-face ((t (:foreground "SeaGreen2"))))
 '(font-lock-warning-face ((t (:foreground "yellow"))))
 '(linum ((t (:background "#101418" :foreground "#606060" :height 0.85))))
 '(vertical-border ((t (:foreground "#101418"))))
 '(button ((t (:inherit (link)))))
 '(link ((((class color) (min-colors 88) (background light)) (:underline (:color foreground-color :style line) :foreground "RoyalBlue3")) (((class color) (background light)) (:underline (:color foreground-color :style line) :foreground "blue")) (((class color) (min-colors 88) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan1")) (((class color) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan")) (t (:inherit (underline)))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(fringe ((t (:background "#405060"))))
 '(header-line ((t (:box nil :foreground "grey90" :background "grey20"))))
 '(tooltip ((t (:foreground "black" :background "lightyellow"))))
 '(mode-line ((((class color) (min-colors 88)) (:foreground "black" :background "grey75" :box (:line-width -1 :color nil :style released-button))) (t (:inverse-video t))))
 '(mode-line-buffer-id ((t (:weight bold :box (:line-width 2 :color nil :style released-button) :foreground "black" :background "gray"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((default (:inherit (mode-line))) (((class color) (min-colors 88) (background light)) (:background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75" :style nil) :weight light)) (((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))
 '(isearch ((t (:foreground "brown4" :background "palevioletred2"))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'ally)
