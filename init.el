;;----------------------------------------------------------------------------
;; Bootstrapp configs
;;----------------------------------------------------------------------------
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/themes")
(setq emacs-directory "~/.emacs.d/")

;;theme
(load-theme 'tomorrow-night t)

;;require config files
(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)
(require 'key-bindings)
(require 'packages)
(require 'visual)


;;----------------------------------------------------------------------------
;; Global modes configs
;;----------------------------------------------------------------------------


;;company
(setq company-dabbrev-code-ignore-case t)
(setq company-dabbrev-downcase nil)
(setq company-idle-delay 0.5)
(add-hook 'after-init-hook 'global-company-mode)
(set (make-local-variable 'company-backends) '(company-tern))
(add-to-list 'company-backends 'company-yasnippet)

;;ido vertical
(ido-vertical-mode 1)

;;ace-jump mode
(setq ace-jump-mode-gray-background nil)

;;undo-tree-mode
(global-undo-tree-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-highlighting-mode 'lines)
(setq flycheck-rust-library-path '("../target/deps"))

;;pbcopy
(require 'pbcopy)
(turn-on-pbcopy)

;;yasnippet
(add-to-list 'load-path (file-expand-wildcards (concat emacs-directory "elpa/yasnippet-*/snippets")))
(require 'yasnippet)
(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))
(yas-global-mode 1)

(projectile-global-mode)


;;make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;auto pair brackets and match them
(electric-pair-mode t)
(show-paren-mode 1)


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

;;mac os x option key as meta
(set-keyboard-coding-system nil)


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

(defun buffer-is-tabbed()
  (if (and (stringp mode-name)
           (or (string-equal (buffer-name) "Makefile")
               (string-equal mode-name "Makefile")
               (string-equal mode-name "Go")
               (string-equal mode-name "BSDmakefile")))
  t))

;;delete whitespaces
(defun cleanup-buffer-safe ()
  (interactive)
  (if (not (buffer-is-tabbed))
      (untabify (point-min) (point-max)))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'cleanup-buffer-safe)

;;tabs instead of spaces except makefiles and go
(add-hook 'makefile-mode-hook '(lambda ()
                                 (setq indent-tabs-mode t)))
(setq-default indent-tabs-mode nil)

;;----------------------------------------------------------------------------
;; Language configs
;;----------------------------------------------------------------------------

;;python
(add-to-list 'company-backends 'company-anaconda)
(add-hook 'python-mode-hook 'anaconda-mode)

;;js2mode fork
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js-mode-hook (lambda ()
  (set (make-local-variable 'company-backends) '(company-tern))
  (company-mode)))

;;sass, css
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;;go
(require 'company-go)
(setq gofmt-command "goimports")
(add-to-list 'load-path "~/.go/misc/emacs/")
(add-hook 'go-mode-hook (lambda ()
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)))
;; (require 'go-mode-load)
;; (add-hook 'before-save-hook 'gofmt-before-save)

;;multi-web-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)
