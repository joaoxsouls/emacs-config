;GLOBAL KEYS

(global-set-key (kbd "M-n") 'backward-paragraph)

(global-set-key (kbd "M-<up>") 'backward-paragraph)

(global-set-key (kbd "M-m") 'forward-paragraph)

(global-set-key (kbd "M-<down>") 'forward-paragraph)

(global-set-key (kbd "M-<right>") 'forward-word)

(global-set-key (kbd "M-<left>") 'backward-word)

(global-set-key (kbd "C-<up>") 'beginning-of-buffer)

(global-set-key (kbd "C-<down>") 'end-of-buffer)

(global-set-key (kbd "C-x x") 'next-multiframe-window)

(global-set-key (kbd "C-x w") 'delete-window)

(global-set-key (kbd "C-z") 'undo-only)

(global-set-key (kbd "C-u") 'lusty-file-explorer)

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

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

(global-set-key (kbd "C-n") 'dirtree-show)

(global-set-key (kbd "C-x b")   'lusty-buffer-explorer)

(provide 'key-bindings)
