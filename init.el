;;emacs configs

(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/")
(load-theme 'tomorrow-night t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;disable menubar/scrollbar/tool-bar
(customize-set-variable 'menu-bar-mode nil)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'tool-bar-mode nil)

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'packages)
(require 'key-bindings)

;;----------------------------------------------------------------------------
;; Global modes configs
;;----------------------------------------------------------------------------

(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;auto pair brackets and match them
(electric-pair-mode t)
(show-paren-mode 1)

;;disable backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;;disable auto save
(setq auto-save-default nil)

;;recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;;disable startup msg
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;;line and column numbers
(global-linum-mode 1)
(setq linum-format " %d ")
(column-number-mode 1)
;;buffer size
(size-indication-mode t)

;; No yes-or-no, y-or-n instead
(defalias 'yes-or-no-p 'y-or-n-p)

;;delete selected text with any key
(delete-selection-mode t)

;; Auto revert buffers
(global-auto-revert-mode 1)

;; revert buffer w/o asking
(setq revert-without-query (quote (".*")))

;;camel case
(add-hook 'prog-mode-hook 'subword-mode)
