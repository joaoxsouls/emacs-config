;html-mode hooks
(add-hook 'html-mode-hook
          (lambda ()
            (local-set-key (kbd "s-b") 'sgml-skip-tag-backward)
            (local-set-key (kbd "s-e") 'sgml-skip-tag-forward)
            (local-set-key (kbd "s-d") 'sgml-delete-tag)
            (local-set-key (kbd "C-c <right>") 'next-multiframe-window)
            (local-set-key (kbd "C-c <left>") 'previous-multiframe-window)
            (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
            ))

;lusty hooks

(add-hook 'lusty-setup-hook 'my-lusty-hook)
(defun my-lusty-hook ()
  (define-key lusty-mode-map "C-<right>" 'lusty-highlight-next)
  (define-key lusty-mode-map "C-<left>" 'lusty-highlight-previous))

;python-mode hooks

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
            (local-set-key (kbd "C-c C-r") 'recentf-open-files)
            ))

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

(provide 'mode-hooks)
