;customizations file
;-------------------

;startup msg
(setq inhibit-startup-message t)

;line and column numbers
(global-linum-mode 1)
(setq linum-format " %d ")
(column-number-mode 1)

;;theme
(load-theme 'most-monokai-cli t)

;mac os x option key as meta
(set-keyboard-coding-system nil)

;shift select up
(if (equal "xterm-256color" (tty-type))
      (define-key input-decode-map "\e[1;2A" [S-up]))

;; No yes-or-no, y-or-n instead
(defalias 'yes-or-no-p 'y-or-n-p)

;delete selected text with any key
(delete-selection-mode t)

;; Auto revert buffers
(global-auto-revert-mode 1)

; revert buffer w/o asking
(setq revert-without-query (quote (".*")))

;freaking whitespaces trail
(defun cleanup-buffer-safe ()
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'cleanup-buffer-safe)

;; ;python
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (require 'pymacs)
;;             (autoload 'pymacs-apply "pymacs")
;;             (autoload 'pymacs-call "pymacs")
;;             (autoload 'pymacs-eval "pymacs" nil t)
;;             (autoload 'pymacs-exec "pymacs" nil t)
;;             (autoload 'pymacs-load "pymacs" nil t)
;;             ;; Initialize Rope
;;             (ac-ropemacs-initialize)
;;             (pymacs-load "ropemacs" "rope-")
;;             (setq ropemacs-enable-autoimport t)

;;             (add-to-list 'ac-sources 'ac-source-ropemacs)))

;disable menubar/scrollbar/tool-bar
(custom-set-variables
 '(blink-cursor-mode nil)
 '(menu-bar-mode nil)
 '(indent-tabs-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tool-bar-mode nil))
(custom-set-faces)

;------------------
;GUI-only  Customizations
;------------------

;face customizations
(if (window-system)
(progn
(global-visual-line-mode)
(put 'upcase-region 'disabled nil)
(global-set-key "\C-cz" 'show-file-name)
(setq transparency-level 90)
;transparency
(set-frame-parameter nil 'alpha transparency-level)
(add-hook 'after-make-frame-functions (lambda (selected-frame) (set-frame-parameter selected-frame 'alpha transparency-level)))
(setq frame-title-format
;file location on statusbar
  '(:eval
    (if buffer-file-name
        (replace-regexp-in-string
         "\\\\" "/"
         (replace-regexp-in-string
          (regexp-quote (getenv "HOME")) "~"
          (convert-standard-filename buffer-file-name)))
      (buffer-name))))
(put 'upcase-region 'disabled nil)
(global-set-key "\C-cz" 'show-file-name)
;font and window customizations
(custom-set-faces
 `(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :foundry "apple" :family "Consolas")))))
(set-frame-width (selected-frame) 130)
(set-frame-height (selected-frame) 40)
(load-theme 'most-monokai-gui t)))

(provide 'customizations)
