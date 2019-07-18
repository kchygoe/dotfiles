;;; whitespace -- setup whitespace
;;; Commentary:
;;; Code:

(use-package whitespace
  :config
  (setq whitespace-style '(face
                           trailing
                           tabs
                           spaces
                           empty
                           space-mark
                           tab-mark
                           ))
  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          ;; WARNING: the mapping below has a problem.
          ;; When a TAB occupies exactly one column, it will display the
          ;; character ?\xBB at that column followed by a TAB which goes to
          ;; the next TAB column.
          ;; If this is a problem for you, please, comment the line below.
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  ;;(setq whitespace-space-regexp "\\(\u3000+\\)")
  (setq whitespace-action '(auto-cleanup))
  (global-whitespace-mode 1))

;; Color for White spaces
(setq whitespace-action '(auto-cleanup))

(defvar my/bg-color "#262626")
(set-face-attribute 'whitespace-trailing nil
		    :background my/bg-color
		    :foreground "DarkBlue"
		    :underline t)
(set-face-attribute 'whitespace-tab nil
		    :background my/bg-color
		    :foreground "LightSkyBlue"
		    :underline t)
(set-face-attribute 'whitespace-space nil
		    :background my/bg-color
		    :foreground "DarkGreen"
		    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
		    :background my/bg-color)


(provide 'init-whitespace)

;;; init-whitespace.el ends here
