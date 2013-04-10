;--PATHS
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'custom-theme-load-path "~/.emacs.d/modes/themes")
(setq default-directory "~/.emacs.d/")
 ;--REQUIRES

;--PACKAGE MANAGER
(require 'package)
(require 'melpa)
(require 'cl)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar package-list
  '(
    auto-complete yasnippet
    js2-mode coffee-mode css-mode scss-mode
    ctable
    concurrent
    deferred
    epc
    flymake-cursor flymake-python-pyflakes
    lusty-explorer
    simp
    )
  "List of packages needs to be installed at launch")


(defun has-package-not-installed ()
  (loop for p in package-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))

(has-package-not-installed)

(when (has-package-not-installed)
  ;; check for new packages
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p package-list)
    (when (not (package-installed-p p))
      (package-install p))))

;config files
(require 'key-bindings)

;--LOAD MODES

(smex-initialize)

;autocomplete mode
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories (concat default-directory "modes/autocomplete/dict"))
(require 'auto-complete-config)
(ac-config-default)

;python pylint pep8
(setq flymake-python-pyflakes-executable "flake8")
(setq flymake-python-pyflakes-extra-arguments '("--ignore=E128,E501,F403"))
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(eval-after-load 'flymake '(require 'flymake-cursor))

;jedi
(setq jedi:setup-keys t)
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)

;copy text from emacs to external app
(require 'pbcopy)
(turn-on-pbcopy)

;js2mode fork
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;coffee-mode
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))


;sass
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;yasnippet
(setq yas-snippet-dirs (concat default-directory "modes/yasnippet/snippets/"))
(yas-global-mode 1)

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

;auto pair brackets
(electric-pair-mode t)

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

;ternjs javascript autocomplete
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))


;startup msg
(setq inhibit-startup-message t)

;line and column numbers
(global-linum-mode 1)
(setq linum-format " %d ")
(column-number-mode 1)

;;theme
(load-theme 'most-monokai-cli t)

;mac os x option key as meta
(set-keyboard-coding-system nil)

;shift select up
(if (equal "xterm-256color" (tty-type))
      (define-key input-decode-map "\e[1;2A" [S-up]))

;; No yes-or-no, y-or-n instead
(defalias 'yes-or-no-p 'y-or-n-p)

;delete selected text with any key
(delete-selection-mode t)

;; Auto revert buffers
(global-auto-revert-mode 1)

; revert buffer w/o asking
(setq revert-without-query (quote (".*")))

;freaking whitespaces trail
(defun cleanup-buffer-safe ()
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'cleanup-buffer-safe)

;disable menubar/scrollbar/tool-bar
(custom-set-variables
 '(blink-cursor-mode nil)
 '(menu-bar-mode nil)
 '(indent-tabs-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tool-bar-mode nil))
(custom-set-faces)

;------------------
;GUI-only  Customizations
;------------------

;face customizations
(if (window-system)
(progn
(global-visual-line-mode)
(put 'upcase-region 'disabled nil)
(global-set-key "\C-cz" 'show-file-name)
(setq transparency-level 90)
;transparency
(set-frame-parameter nil 'alpha transparency-level)
(add-hook 'after-make-frame-functions (lambda (selected-frame) (set-frame-parameter selected-frame 'alpha transparency-level)))
(setq frame-title-format
;file location on statusbar
  '(:eval
    (if buffer-file-name
        (replace-regexp-in-string
         "\\\\" "/"
         (replace-regexp-in-string
          (regexp-quote (getenv "HOME")) "~"
          (convert-standard-filename buffer-file-name)))
      (buffer-name))))
(put 'upcase-region 'disabled nil)
(global-set-key "\C-cz" 'show-file-name)
;font and window customizations
(custom-set-faces
 `(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :foundry "apple" :family "Consolas")))))
(set-frame-width (selected-frame) 130)
(set-frame-height (selected-frame) 40)
(load-theme 'most-monokai-gui t)))
