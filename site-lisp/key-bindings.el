(defun irb ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-region (point-min) (point-max))
        (message "Indented buffer.")))))

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

(global-set-key (kbd "C-<right>") 'end-of-visual-line)

(global-set-key (kbd "C-<left>") 'beginning-of-visual-line)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "s-r") 'replace-string)

(global-set-key (kbd "C-M-<right>") 'forward-sexp)

(global-set-key (kbd "C-M-<left>") 'backward-sexp)

(global-set-key (kbd "<return>") 'newline-and-indent)

(provide 'key-bindings)
