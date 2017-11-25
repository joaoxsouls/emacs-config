(setq use-package-always-ensure t)

(use-package evil-goggles)
(use-package evil
  :init
  (setq foo-variable t)
  :config
  (unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate))
  (define-key evil-motion-state-map (kbd "SPC") #'evil-avy-goto-word-or-subword-1)
  (evil-mode 1)
  (evil-goggles-mode))

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))
;;(add-hook 'before-save-hook 'editorconfig-format-buffer)

(use-package avy
  :defer t
  :config
  (setq avy-keys (number-sequence ?a ?z)))

(use-package projectile
  :config
  (projectile-global-mode t)
  :diminish projectile-mode
  :bind*
  ("C-c C-p" . projectile-find-file))

;;ido
(use-package ido-vertical-mode)
(use-package flx-ido)
(use-package ido
  :init
  (flx-ido-mode)
  (ido-vertical-mode)
  (ido-everywhere)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  :config
  (ido-mode))

(use-package smex
  :bind ("M-x" . smex))

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-visualizer-timestamps t))

(use-package pbcopy
  :config
  (turn-on-pbcopy))

(use-package expand-region
  :bind*
  (("M-1" . er/contract-region)
  ("M-2" . er/expand-region)))

;;----------------------------------------------------------------------------
;;LANGUAGES
;;----------------------------------------------------------------------------
(use-package company
  :diminish company-mode
  :init
  (setq company-dabbrev-code-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.5)
  (setq company-dabbrev-code-ignore-case t)
  (setq company-dabbrev-downcase nil)
  :bind*
  ("C-M-i" . company-complete)
  :config
  (global-company-mode t))

(use-package company-anaconda
  :after (anaconda-mode company)
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package anaconda-mode
  :diminish anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  :bind
  (("C-d" . anaconda-mode-find-definitions)
   ("C-f" . anaconda-mode-go-back)))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package web-mode
  :mode
  "\\.html\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package rust-mode
  :mode
  "\\.rs\\'")

(use-package racer
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode))

(use-package flycheck-rust
  :after (rust-mode flycheck-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rustfmt
  :disabled t
  :after rust
  :bind (("C-c C-f" . rustfmt-format-buffer)))

(use-package flycheck
  :diminish flycheck-mode
  :init
  (setq flycheck-highlighting-mode 'lines)
  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point)
  (global-flycheck-mode))

(use-package eclim
  :init
  (setq eclimd-autostart t)
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  :config
  (add-hook 'java-mode-hook (lambda () (eclim-mode t)))
  (help-at-pt-set-timer))

(provide 'packages)
