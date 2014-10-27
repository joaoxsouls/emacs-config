;;viual configs

;;disable menubar/scrollbar/tool-bar
(custom-set-variables
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))


;;------------------
;;GUI-only  Customizations
;;------------------

;;face customizations
(if (window-system)
    (progn
      (global-visual-line-mode)
      (put 'upcase-region 'disabled nil)
      (global-set-key "\C-cz" 'show-file-name)
      (setq transparency-level 90)
      ;;transparency
      (set-frame-parameter nil 'alpha transparency-level)
      (add-hook 'after-make-frame-functions (lambda (selected-frame) (set-frame-parameter selected-frame 'alpha transparency-level)))
      (setq frame-title-format
            ;;file location on statusbar
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
      ;;font and window customizations
      (custom-set-faces
       `(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :foundry "apple" :family "Inconsolata")))))
      (set-cursor-color "#ffffff")
      (set-frame-width (selected-frame) 130)
      (set-frame-height (selected-frame) 40)
      )
  )

(provide 'visual)
