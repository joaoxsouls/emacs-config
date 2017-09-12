
;;rename files
(defun rf ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rf)

;;delete files
(defun df ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'df)

(defun cc ()
  "comment or uncomment a region if selected, otherwise the whole line"
  (interactive)
  (save-excursion
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
(global-set-key (kbd "C-c C-c") 'cc)

(defun ib ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun irb ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (ib)
        (message "Indented buffer.")))))

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(global-set-key (kbd "C-c n") 'cleanup-buffer)

;;ido recentfiles
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-c C-r") 'recentf-ido-find-file)

;;rgrep project
(eval-after-load "grep"
  '(grep-compute-defaults))
(defun rgrep-project()
  (interactive)
  (rgrep (grep-read-regexp) "*.*" (simp-project-root))
  )

;;eclim-mode hooks
(add-hook 'eclim-mode-map-hook
          (lambda ()
            (local-set-key (kbd "C-M-i") 'company-emacs-eclim)
            ))

;;python-mode keys
;;hook functions
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-d") 'anaconda-mode-find-definitions)
            (local-set-key (kbd "C-f") 'anaconda-mode-go-back)
            (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)
            (local-set-key (kbd "C-c C-r") 'recentf-ido-find-file)
            (local-set-key (kbd "C-c C-p") 'projectile-find-file)
            ))

;;html-mode hooks
(add-hook 'html-mode-hook
          (lambda ()
            (local-set-key (kbd "s-b") 'sgml-skip-tag-backward)
            (local-set-key (kbd "s-e") 'sgml-skip-tag-forward)
            (local-set-key (kbd "s-d") 'sgml-delete-tag)
            (local-set-key (kbd "C-c <right>") 'next-multiframe-window)
            (local-set-key (kbd "C-c <left>") 'previous-multiframe-window)
            (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)
            ))


;;elixir-mode and erlang-mode hooks
(add-hook 'elixir-mode-hook
          (lambda ()
            (local-set-key (kbd "C-d") 'alchemist-goto-definition-at-point)
            (local-set-key (kbd "C-f") 'alchemist-goto-jump-back)
            ))

(add-hook 'erlang-mode-hook
          (lambda ()
            (local-set-key (kbd "C-d") 'alchemist-goto-definition-at-point)
            (local-set-key (kbd "C-f") 'alchemist-goto-jump-back)
            ))

(add-hook 'magit-mode-hook
          (lambda ()
            (local-set-key (kbd "r") 'magit-refresh)
            (local-set-key (kbd "<down>") 'magit-section-forward)
            (local-set-key (kbd "<up>") 'magit-section-backward)
            ))

;;ensime keys
(add-hook 'ensime-mode-hook
          (lambda ()
            (local-set-key (kbd "C-d") 'ensime-edit-definition)
            (local-set-key (kbd "C-f") 'ensime-pop-find-definition-stack)
            (local-set-key (kbd "C-c C-p") 'projectile-find-file)
            (local-set-key (kbd "C-c C-r") 'recentf-ido-find-file)
            ))

;;java-mode
(add-hook 'java-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-p") 'projectile-find-file)
            ))

;;GLOBAL KEYS

(global-set-key (kbd "M-n") 'backward-paragraph)

(global-set-key (kbd "M-<up>") 'backward-paragraph)

(global-set-key (kbd "M-m") 'forward-paragraph)

(global-set-key (kbd "M-<down>") 'forward-paragraph)

(global-set-key (kbd "M-<right>") 'forward-word)

(global-set-key (kbd "M-<left>") 'backward-word)

(global-set-key (kbd "C-<up>") 'beginning-of-buffer)

(global-set-key (kbd "C-<down>") 'end-of-buffer)

(global-set-key (kbd "C-z") 'undo-only)

(global-set-key (kbd "C-x C-f") 'ido-find-file)

(global-set-key (kbd "C-c C-p") 'projectile-find-file)

(global-set-key (kbd "C-c <right>") 'next-multiframe-window)

(global-set-key (kbd "C-c <left>") 'previous-multiframe-window)

(global-set-key (kbd "C-a") 'mark-whole-buffer)

(global-set-key (kbd "C-<right>") 'end-of-visual-line)

(global-set-key (kbd "C-<left>") 'beginning-of-visual-line)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "s-r") 'replace-string)

(global-set-key (kbd "s-u") 'ido-find-file)

(global-set-key (kbd "C-x b") 'ido-switch-buffer)

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

(global-set-key (kbd "C-M-<right>") 'forward-sexp)

(global-set-key (kbd "C-M-<left>") 'backward-sexp)

(global-set-key (kbd "s-+") 'text-scale-increase)

(global-set-key (kbd "s--") 'text-scale-decrease)

(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "M-.") 'mc/mark-all-like-this)

(global-set-key (kbd "M-SPC") 'avy-goto-line)

(global-set-key (kbd "<return>") 'newline-and-indent)

(global-set-key (kbd "C-b") 'set-mark-command)

(global-set-key (kbd "C-@") 'avy-goto-word-or-subword-1)

(global-set-key (kbd "C-SPC") 'avy-goto-word-or-subword-1)

(global-set-key (kbd "M-1") 'er/contract-region)

(global-set-key (kbd "M-2") 'er/expand-region)

(global-set-key (kbd "C-n") 'mc/mark-next-like-this)

(global-set-key (kbd "C-p") 'mc/mark-previous-like-this)

(global-set-key (kbd "M-d") 'comment-or-uncomment-region-or-line)
(provide 'key-bindings)
