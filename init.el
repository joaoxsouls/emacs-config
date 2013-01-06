;--PATHS
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'custom-theme-load-path "~/.emacs.d/modes/themes")
;--REQUIRES

;config files
(require 'key-bindings)
(require 'customizations)
(require 'mode-hooks)

;copy text from emacs to external app
(require 'pbcopy)
(turn-on-pbcopy)

;js2mode fork
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/modes/yasnippet/snippets/"
        ))
(yas-global-mode 1)


(require 'django-html-mode)

;simp
(require 'simp)
(simp-project-define
 '(:has (.git)
   :ignore (.git)))
(require 'ido-better-flex)
(ido-better-flex/enable)

;uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;autocomplete mode
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/modes/autocomplete/dict")
(require 'auto-complete-config)
(ac-config-default)

;line numbers
(require 'linum)
(global-linum-mode 1)
(setq linum-format " %d ")

;lusty mode
(require 'lusty-explorer)

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

(put 'narrow-to-region 'disabled nil)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
