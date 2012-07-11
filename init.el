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

;uniquify
(require 'uniquify)

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
(require 'yasnippet)
(setq yas/root-directory "~/.emacs.d/modes/yasnippet/snippets/")
(yas/load-directory yas/root-directory)
(yas/initialize)

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

