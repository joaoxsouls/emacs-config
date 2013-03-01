;--PATHS
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'custom-theme-load-path "~/.emacs.d/modes/themes")
;--REQUIRES

(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;config files
(require 'key-bindings)
(require 'customizations)
(require 'mode-hooks)

;EPC
(require 'epc)

;autocomplete mode
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/modes/autocomplete/dict")
(require 'auto-complete-config)
(ac-config-default)

;jedi
(setq jedi:setup-keys t)
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:complete-on-dot t)

;copy text from emacs to external app
(require 'pbcopy)
(turn-on-pbcopy)

;js2mode fork
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;python mode
(require 'python)

;sass
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

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
(setq ido-enable-flex-matching t)

;uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;line numbers
(require 'linum)
(global-linum-mode 1)
(setq linum-format " %d ")

;lusty mode
(require 'lusty-explorer)

;auto pair brackets
(electric-pair-mode t)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-safe-themes (quote ("c17eba2c8a017959699591f0cbb4739afb3ebb891c0887e58ad95cf667859253" default)))
 '(indent-tabs-mode nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
