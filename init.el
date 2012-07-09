;--PATHS
(let ((default-directory "~/.emacs.d/"))
      (normal-top-level-add-subdirs-to-load-path))

;--REQUIRES

;config files
(require 'key-bindings)
(require 'customizations)
(require 'mode-hooks)

;copy text from emacs to external app
(require 'pbcopy)
(turn-on-pbcopy)

;theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-almost-monokai)

;autocomplete mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/modes/autocomplete/dict")
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
(setq ac-ignore-case nil)

;line numbers
(require 'linum)
(global-linum-mode 1)
(setq linum-format " %d ")

;lusty mode
(require 'lusty-explorer)

;yasnippet
(add-to-list 'load-path
              "~/.emacs.d/modes/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/global-mode 1)
(yas/load-directory "~/.emacs.d/modes/yasnippet/snippets/")
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(setq auto-mode-alist (append '(("\\.css$" . css-mode)) auto-mode-alist))

;autopair mode
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;dirtree
(require 'dirtree)

;iswitchb
(iswitchb-mode 1)

;matching braces
(show-paren-mode 1)

;delete selected text with any key 
(delete-selection-mode t)

;mouse support on cli
(xterm-mouse-mode)

