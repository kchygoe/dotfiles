;;; golang -- init golang enva
;;; Code:
;;; Commentary:

(add-to-list 'exec-path (expand-file-name "~/.go/bin"))
(use-package go-mode)
(use-package company-go)
(use-package go-autocomplete)
(use-package go-flymake)
(use-package lsp-go
  :after (lsp-mode go-mode)
  :custom (lsp-go-language-server-flags '(
    "-gocodecompletion"
    "-diagnostics"
    "-lint-tool=golint"))
  :hook (go-mode . lsp-go-enable)
  :commands lsp-go-enable)

;; (add-hook 'go-mode-hook (lambda()
;;                           (add-hook 'before-save-hook 'gofmt-before-save)
;;                           (local-set-key (kbd "M-.") 'godef-jump)
;;                           (set (make-local-variable 'company-backends) '(company-go))
;;                           (company-mode)
;;                           (setq c-basic-offset 2)
;;                           (setq tab-width 2)))
(provide 'init-golang)

;;; init-golang.el ends here
