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

;coffee-mode
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))


;sass
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

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

;make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;lusty mode
(require 'lusty-explorer)

;auto pair brackets
(electric-pair-mode t)

;dirtree
(require 'dirtree)

;matching braces
(show-paren-mode 1)

;mouse support on cli
(xterm-mouse-mode)

;disable backup files
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
