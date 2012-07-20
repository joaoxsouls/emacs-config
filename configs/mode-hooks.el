
;javascript mode hooks
(add-hook 'javascript-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ;; Activate the folding mode
            (hs-minor-mode t)))


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


(provide 'mode-hooks)
