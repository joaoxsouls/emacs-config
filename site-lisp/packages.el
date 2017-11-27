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
  :diminish projectile-mode)

(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (helm-mode)
  :bind
  (("M-x" . helm-M-x)
   ("C-x b" . helm-mini)
   ("C-x C-b" . helm-mini)
   ("C-x C-f" . helm-find-files)))
(use-package helm-projectile
  :after projectile
  :bind*
  ("C-c C-p" . helm-projectile)
  ("C-c p s s " . helm-projectile-ack))

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


(use-package crux
  :bind
  ("C-x C-r" . crux-rename-file-and-buffer)
  ("C-x C-k" . crux-delete-file-and-buffer))

;;----------------------------------------------------------------------------
;;LANGUAGES
;;----------------------------------------------------------------------------
(use-package flycheck
  :diminish flycheck-mode
  :init
  (setq flycheck-highlighting-mode 'lines)
  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point)
  (global-flycheck-mode))

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

(use-package eclim
  :init
  (help-at-pt-set-timer)
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (setq auto-save-default nil)
  (setq eclim-auto-save nil)
  :config
  (add-hook 'java-mode-hook (lambda () (eclim-mode t) (start-eclimd "~/workspace"))))

(use-package company-emacs-eclim
  :after eclim
  :config
  (company-emacs-eclim-setup))

(provide 'packages)
