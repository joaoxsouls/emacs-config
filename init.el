;;----------------------------------------------------------------------------
;; Bootstrapp configs
;;----------------------------------------------------------------------------

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)      

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
(require 'evil-terminal-cursor-changer)
(require 'key-bindings)
(require 'packages)
(require 'visual)


;;----------------------------------------------------------------------------
;; Global modes configs
;;----------------------------------------------------------------------------

;;evil-mode
(use-package evil
  :init
  (setq foo-variable t)
  :config
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate))
  (define-key evil-motion-state-map (kbd "SPC") #'evil-avy-goto-word-or-subword-1)
  (evil-mode 1))

;;company
(use-package company
  :init
  (setq company-dabbrev-code-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.5)
  (setq company-dabbrev-code-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.5)
  (global-company-mode t)
  :config
  (add-to-list 'company-backends 'company-yasnippet 'company-emacs-eclim)
  )

;;editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;ido
(ido-vertical-mode 1)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;;undo-tree-mode
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-highlighting-mode 'lines)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

;;pbcopy
(require 'pbcopy)
(turn-on-pbcopy)

;;yasnippet
;; (add-to-list 'load-path (file-expand-wildcards (concat emacs-directory "elpa/yasnippet-*/snippets")))
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

;;avy-mode
(setq avy-keys (number-sequence ?a ?z))

;;hight-symbol-mode
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(defun buffer-is-tabbed()
  (if (and (stringp mode-name)
           (or (string-equal (buffer-name) "Makefile")
               (string-equal mode-name "Makefile")
               (string-equal mode-name "Go")
               (string-equal mode-name "BSDmakefile")))
      t))

;;delete whitespaces
;; (defun cleanup-buffer-safe ()
;;   (interactive)
;;   (if (not (buffer-is-tabbed))
;;       (untabify (point-min) (point-max)))
;;   (delete-trailing-whitespace)
;;   (set-buffer-file-coding-system 'utf-8))

;; (add-hook 'before-save-hook 'cleanup-buffer-safe)

;;popwin mode
(require 'popwin)
(popwin-mode 1)

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
;; (require 'multi-web-mode)
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;; (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;; (jsx-mode "<script +\\(type=\"text/jsx\"\\|language=\"jsx\"\\)[^>]*>" "</script>")
;; (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("php" "htm" "html" "eex" "ctp" "phtml" "php4" "php5"))
;; (multi-web-global-mode 1)

;;elixir-mode and erlang-mode
(add-hook 'elixir-mode-hook 'alchemist-mode)
(add-hook 'erlang-mode-hook 'alchemist-mode)

;;rust
(setq racer-cmd "/Users/jxs/dev/jxs/racer/target/release/racer")
(setq racer-rust-src-path "/Users/jxs/dev/jxs/racer/target/src/rustc-1.3.0/src")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)


(defvar projectile-mode-line
  '(:propertize
    (:eval (when (ignore-errors (projectile-project-root))
             (concat " " (projectile-project-name))))
    face font-lock-constant-face)
  "Mode line format for Projectile.")
(put 'projectile-mode-line 'risky-local-variable t)

(defvar vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face font-lock-variable-name-face))
  "Mode line format for VC Mode.")
(put 'vc-mode-line 'risky-local-variable t)

(setq-default mode-line-format
              '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                ;; Some specific information about the current buffer:
                projectile-mode-line ; Project information
                (vc-mode vc-mode-line)
                (flycheck-mode flycheck-mode-line) ; Flycheck status
                (multiple-cursors-mode mc/mode-line) ; Number of cursors
                " "
                mode-line-misc-info
                " " mode-line-end-spaces))
