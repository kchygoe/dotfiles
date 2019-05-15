;;; golang -- init golang enva
;;; Code:
;;; Commentary:

(add-to-list 'exec-path (expand-file-name "~/.go/bin"))
(use-package go-mode)
(use-package company-go)

;; (add-hook 'go-mode-hook (lambda()
;;                           (add-hook 'before-save-hook 'gofmt-before-save)
;;                           (local-set-key (kbd "M-.") 'godef-jump)
;;                           (set (make-local-variable 'company-backends) '(company-go))
;;                           (company-mode)
;;                           (setq c-basic-offset 2)
;;                           (setq tab-width 2)))
(provide 'init-golang)

;;; init-golang.el ends here
