
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

(global-set-key (kbd "M-n") 'backward-paragraph)

(global-set-key (kbd "M-<up>") 'backward-paragraph)

(global-set-key (kbd "M-m") 'forward-paragraph)

(global-set-key (kbd "M-<down>") 'forward-paragraph)

(global-set-key (kbd "M-<right>") 'forward-word)

(global-set-key (kbd "M-<left>") 'backward-word)

(global-set-key (kbd "C-<up>") 'beginning-of-buffer)

(global-set-key (kbd "C-<down>") 'end-of-buffer)

(global-set-key (kbd "C-c <right>") 'next-multiframe-window)

(global-set-key (kbd "C-c <left>") 'previous-multiframe-window)

(global-set-key (kbd "C-a") 'mark-whole-buffer)

(global-set-key (kbd "C-<right>") 'end-of-visual-line)

(global-set-key (kbd "C-<left>") 'beginning-of-visual-line)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "s-r") 'replace-string)

(global-set-key (kbd "C-M-<right>") 'forward-sexp)

(global-set-key (kbd "C-M-<left>") 'backward-sexp)

(global-set-key (kbd "<return>") 'newline-and-indent)

(provide 'key-bindings)
