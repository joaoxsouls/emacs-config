

;new line after and before the current
(defun open-line-below ()
  (interactive)
  (save-excursion
    (end-of-line)
    (newline)
    (indent-for-tab-command)))

(defun open-line-above ()

  (interactive)
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (newline)
    (indent-for-tab-command)))

(global-set-key (kbd "C-c C-b") 'open-line-below)
(global-set-key (kbd "C-c C-a") 'open-line-above)

;rename files
(defun rename-current-buffer-file ()
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

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;delete files
(defun delete-current-buffer-file ()
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

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun comment-or-uncomment-region-or-line ()
  "comment or uncomment a region if selected, otherwise the whole line"
  (interactive)
  (save-excursion
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)

;copy region or line
(defun copy-region-or-whole-line (arg)
  (interactive "p")
  (save-excursion)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))
  (global-set-key (kbd "M-w") 'copy-region-or-whole-line)

;kill region or line
(defun kill-region-or-whole-line (&optional arg)
  (interactive "p")
  (save-excursion
    (if (region-active-p)
          (kill-region (region-beginning) (region-end))
            (kill-whole-line arg))))
(global-set-key (kbd "C-w") 'kill-region-or-whole-line)

(defun delete-region-or-whole-line(&optional arg)
  (interactive "p")
  (save-excursion
  (if (region-active-p)
  (delete-region (region-beginning) (region-end))
  (delete-region (line-beginning-position) (line-end-position)))))
(global-set-key (kbd "C-k") 'delete-region-or-whole-line)

;clean-up and ident
(defun cleanup-buffer ()
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(global-set-key (kbd "C-c n") 'cleanup-buffer)

;ido recentfiles
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-c C-r") 'recentf-ido-find-file)

;python-mode keys
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-d") 'jedi:goto-definition)
            (local-set-key (kbd "C-M-i") 'jedi:complete)
            (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)
            (local-set-key (kbd "C-c C-r") 'recentf-ido-find-file)
            (local-set-key (kbd "C-c C-p") 'simp-project-find-file)
            ))

;html-mode hooks
(add-hook 'html-mode-hook
          (lambda ()
            (local-set-key (kbd "s-b") 'sgml-skip-tag-backward)
            (local-set-key (kbd "s-e") 'sgml-skip-tag-forward)
            (local-set-key (kbd "s-d") 'sgml-delete-tag)
            (local-set-key (kbd "C-c <right>") 'next-multiframe-window)
            (local-set-key (kbd "C-c <left>") 'previous-multiframe-window)
            (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)
            ))

;lusty keys

;GLOBAL KEYS

(global-set-key (kbd "M-n") 'backward-paragraph)

(global-set-key (kbd "M-<up>") 'backward-paragraph)

(global-set-key (kbd "M-m") 'forward-paragraph)

(global-set-key (kbd "M-<down>") 'forward-paragraph)

(global-set-key (kbd "M-<right>") 'forward-word)

(global-set-key (kbd "M-<left>") 'backward-wod)

(global-set-key (kbd "C-<up>") 'beginning-of-buffer)

(global-set-key (kbd "C-<down>") 'end-of-buffer)

(global-set-key (kbd "C-x x") 'next-multiframe-window)

(global-set-key (kbd "C-x w") 'delete-window)

(global-set-key (kbd "C-z") 'undo-only)

(global-set-key (kbd "C-x C-f") 'lusty-file-explorer)

(global-set-key (kbd "C-C C-p") 'simp-project-find-file)

(global-set-key (kbd "C-c <right>") 'next-multiframe-window)

(global-set-key (kbd "C-c <left>") 'previous-multiframe-window)

(global-set-key (kbd "C-a") 'mark-whole-buffer)

(global-set-key (kbd "C-<right>") 'end-of-visual-line)

(global-set-key (kbd "C-<left>") 'beginning-of-visual-line)

(global-set-key (kbd "C-x u") 'undo-only)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "s-d") 'hs-toggle-hiding)

(global-set-key (kbd "s-r") 'replace-string)

(global-set-key (kbd "s-u") 'lusty-file-explorer)

(global-set-key (kbd "C-n") 'dirtree-show)

(global-set-key (kbd "C-x b")   'lusty-buffer-explorer)

(global-set-key (kbd "C-c s") 'desktop-save)

(global-set-key (kbd "C-c l") 'desktop-change-dir)

(global-set-key (kbd "C-c C-s") 'split-window-horizontally)

(global-set-key (kbd "C-M-<down>") 'scroll-up-command)

(global-set-key (kbd "C-M-<up>") 'scroll-down-command)

(global-set-key (kbd "C-+") 'text-scale-increase)

(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "M-.") 'mc/mark-all-like-this)

(global-set-key (kbd "M-SPC") 'ace-jump-line-mode)

(global-set-key (kbd "C-@") 'ace-jump-mode)

(global-set-key (kbd "C-e") 'er/expand-region)

(global-set-key (kbd "RET") 'newline-and-indent)

(provide 'key-bindings)
