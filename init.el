;;--PATHS
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'custom-theme-load-path "~/.emacs.d/modes/themes")
(setq emacs-directory "~/.emacs.d/")
;;--REQUIRES

;;--PACKAGE MANAGER
(require 'package)
(require 'melpa)
(require 'cl)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar package-list
  '(
    smex
    auto-complete
    yasnippet
    web-mode
    js2-mode
    coffee-mode
    css-mode
    scss-mode
    sass-mode
    puppet-mode
    markdown-mode
    go-mode
    go-autocomplete
    ctable
    concurrent
    deferred
    jedi
    epc
    flymake-cursor
    flymake-python-pyflakes
    flymake-ruby
    robe
    lusty-explorer
    expand-region
    ;;simp
    key-chord
    multiple-cursors
    ace-jump-mode
    multi-web-mode
    undo-tree
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

;;config files
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

;;ace-jump mode
(setq ace-jump-mode-gray-background nil)

;;undo-tree-mode
(global-undo-tree-mode)

;;python pylint pep8
(setq flymake-python-pyflakes-executable "flake8")
(setq flymake-python-pyflakes-extra-arguments '("--ignore=E128,E501,F403"))
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(eval-after-load 'flymake '(require 'flymake-cursor))

;;jedi
(setq jedi:setup-keys t)
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)

;;ruby flymake
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook 'robe-mode)
(push 'ac-source-robe ac-sources)

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
(add-hook 'css-mode-hook 'flymake-mode)
(add-hook 'scss-mode-hook 'flymake-mode)

;;go
(require 'go-autocomplete)
(require 'go-flymake)

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


;;simp
(require 'simp)
(simp-project-define
 '(:has (.git)
        :ignore (.git)))
(setq ido-enable-flex-matching t)

;;make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;auto pair brackets
(electric-pair-mode t)


;;matching braces
(show-paren-mode 1)

;;mouse support on cli
(xterm-mouse-mode)

;;disable backup files
(setq backup-inhibited t)
;;disable auto save
(setq auto-save-default nil)

;;recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;;ternjs javascript autocomplete
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;;startup msg
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
;;sasdads
;;line and column numbers
(global-linum-mode 1)
(setq linum-format " %d ")
(column-number-mode 1)
;;theme
(load-theme 'most-monokai-cli t)

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

;;freaking whitespaces trail
(defun cleanup-buffer-safe ()
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'cleanup-buffer-safe)

;;camel case
(add-hook 'prog-mode-hook 'subword-mode)

;;disable menubar/scrollbar/tool-bar
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
      (load-theme 'most-monokai-gui t))
  )
