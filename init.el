
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'custom-theme-load-path "~/.emacs.d/modes/themes")
(setq emacs-directory "~/.emacs.d/")
;;--REQUIRES

;;--PACKAGE MANAGER
(require 'init-elpa)

(require-package 'smex)
(require-package 'auto-complete)
(require-package 'yasnippet)
(require-package 'js2-mode)
(require-package 'coffee-mode)
(require-package 'css-mode)
(require-package 'scss-mode)
(require-package 'sass-mode)
(require-package 'multi-web-mode)
(require-package 'puppet-mode)
(require-package 'markdown-mode)
(require-package 'go-mode)
(require-package 'go-autocomplete)
(require-package 'rust-mode)
(require-package 'jade-mode)
(require-package 'ctable)
(require-package 'concurrent)
(require-package 'deferred)
(require-package 'jedi)
(require-package 'epc)
(require-package 'robe)
(require-package 'lusty-explorer)
(require-package 'expand-region)
(require-package 'ace-jump-mode)
(require-package 'key-chord)
(require-package 'undo-tree)
(require-package 'multiple-cursors)
(require-package 'projectile)
(require-package 'ido-vertical-mode)

;;Config files
(require 'key-bindings)

;;--LOAD MODES
(require 'smex)
(smex-initialize)

;;autocomplete mode
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat emacs-directory "modes/autocomplete/dict"))
(ac-config-default)
(setq-default ac-sources '(
                           ac-source-yasnippet
                           ac-source-abbrev
                           ac-source-dictionary
                           ac-source-words-in-same-mode-buffers
                           ))

;;ido vertical
(ido-vertical-mode 1)

;;ace-jump mode
(setq ace-jump-mode-gray-background nil)

;;undo-tree-mode
(global-undo-tree-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)

;;jedi
(setq jedi:setup-keys t)
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)

;;copy text from emacs to external app
(require 'pbcopy)
(turn-on-pbcopy)

;;js2mode fork
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;coffee-mode
;; (defun coffee-custom ()
;;   "coffee-mode-hook"
;;   (set (make-local-variable 'tab-width) 2))

(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))


;;sass, css
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;;go
(require 'go-autocomplete)
(setq gofmt-command "goimports")
(add-to-list 'load-path "~/.go/misc/emacs/")
(require 'go-mode-load)
(add-hook 'before-save-hook 'gofmt-before-save)

;;yasnippet
(setq yas-snippet-dirs (concat emacs-directory "modes/yasnippet/snippets/"))
(define-key popup-menu-keymap (kbd "TAB") 'popup-select)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-select)
(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))
(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))
(yas-global-mode 1)

(projectile-global-mode)


;;make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;auto pair brackets
(electric-pair-mode t)
(setq electric-pair-pairs '(
                            (?\' . ?\')
                            (?\" . ?\")
                            ) )

;; ;;matching braces
;; (show-paren-mode 1)

;; (smartparens-global-mode t)

;;mouse support on cli
(xterm-mouse-mode)

;;disable backup files
(setq backup-inhibited t)
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

;;theme
(load-theme 'tomorrow-night t)

;;mac os x option key as meta
(set-keyboard-coding-system nil)

;;shift select up
(if (equal "xterm-256color" (tty-type))
    (define-key input-decode-map "\e[1;2A" [S-up]))

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

(defun buffer-is-makefile()
  (if (or (string-equal (buffer-name) "Makefile")
          (string-equal mode-name "Makefile")
          (string-equal mode-name "BSDmakefile"))
  t))

;;freaking whitespaces trail
(defun cleanup-buffer-safe ()
  (interactive)
  (if (not (buffer-is-makefile))
      (untabify (point-min) (point-max)))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'cleanup-buffer-safe)

;;tabs instead of spaces except makefiles
(add-hook 'makefile-mode-hook '(lambda ()
                                 (setq indent-tabs-mode t)))

;;disable menubar/scrollbar/tool-bar
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#c5c8c6"])
 '(ansi-term-color-vector
   [unspecified "#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#c5c8c6"])
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("2f1998c984911032deae4b07575a22062eee053ea9f8609b700bb510a2e7c053" "936ad15ac8a8f1c79c1b9d6c27ef106bbdce15840e548eda608d43a0666c2a66" "55a34c5a2eae71e9d02cd3143aa6a2fb4af6ca146079719be191477e84e03d8a" "5e2ade7f65d9162ca2ba806908049fb37d602d59d90dc3a08463e1a042f177ae" "9ddd48aea45477dbfa9207e34d2935aa57a8fa9290c33a437dc638bd0e952aea" "c17eba2c8a017959699591f0cbb4739afb3ebb891c0887e58ad95cf667859253" default)))
 '(menu-bar-mode nil)
 '(safe-local-variable-values (quote ((codiing . utf-8))))
 '(scroll-bar-mode nil)
 '(setq indent-tabs-mode)
 '(show-paren-mode t)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:foreground "red" :weight bold))))
 '(flymake-warnline ((t (:foreground "gold" :weight bold)))))

;;keychord mode

(key-chord-mode +1)

;;multi-web-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;;------------------
;;GUI-only  Customizations
;;------------------

;;face customizations
(if (window-system)
    (progn
      (global-visual-line-mode)
      (put 'upcase-region 'disabled nil)
      (global-set-key "\C-cz" 'show-file-name)
      (setq transparency-level 90)
      ;;transparency
      (set-frame-parameter nil 'alpha transparency-level)
      (add-hook 'after-make-frame-functions (lambda (selected-frame) (set-frame-parameter selected-frame 'alpha transparency-level)))
      (setq frame-title-format
            ;;file location on statusbar
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
      ;;font and window customizations
      (custom-set-faces
       `(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :foundry "apple" :family "Inconsolata")))))
      (set-cursor-color "#ffffff")
      (set-frame-width (selected-frame) 130)
      (set-frame-height (selected-frame) 40)
      )
  )
