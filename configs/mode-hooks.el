
;html-mode hooks
(add-hook 'html-mode-hook
          (lambda ()
            (global-set-key (kbd "s-b") 'sgml-skip-tag-backward)
            (global-set-key (kbd "s-e") 'sgml-skip-tag-forward)
            (global-set-key (kbd "s-d") 'sgml-delete-tag)
            ))

;lusty hooks

(add-hook 'lusty-setup-hook 'my-lusty-hook)
(defun my-lusty-hook ()
  (define-key lusty-mode-map "C-<right>" 'lusty-highlight-next)
  (define-key lusty-mode-map "C-<left>" 'lusty-highlight-previous))

;python-mode hooks

(add-hook 'python-mode-hook 'jedi:setup)

(provide 'mode-hooks)
