
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

;iswitch hooks
(defun iswitchb-local-keys ()
  (mapc (lambda (K) 
	(let* ((key (car K)) (fun (cdr K)))
	  (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
      '(("<right>" . iswitchb-next-match)
	("<left>"  . iswitchb-prev-match)
	("<up>"    . ignore             )
	("<down>"  . ignore             ))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)


(provide 'mode-hooks)
