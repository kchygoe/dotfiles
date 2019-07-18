;;; golang -- init golang enva
;;; Code:
;;; Commentary:

(add-to-list 'exec-path (expand-file-name "~/.go/bin"))
(use-package go-mode)
(use-package company-go)
(use-package go-autocomplete)
;(use-package go-flymake)
(use-package lsp-go
  :after (lsp-mode go-mode)
  :custom (lsp-go-language-server-flags '(
    "-gocodecompletion"
    "-diagnostics"
    "-lint-tool=golint"))
  :hook (go-mode . lsp-go-enable)
  :commands lsp-go-enable)

(add-hook 'go-mode-hook (lambda()
                          (company-mode)
                          (setq company-transformers '(company-sort-by-backend-importance))
                          (setq company-minimum-prefix-length 3)
                          (setq completion-ignore-case t)
                          (setq company-dabbrev-downcase nil)
                          (global-set-key (kbd "C-M-i") 'company-complete)
                          (define-key company-active-map (kbd "C-n") 'company-select-next)
                          (define-key company-active-map (kbd "C-p") 'company-select-previous)
                          (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
                          (setq company-idle-delay 0)
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          (local-set-key (kbd "M-.") 'godef-jump)
                          (set (make-local-variable 'company-backends) '(company-go))
                          (setq tab-width 2)))
(provide 'init-golang)

;;; init-golang.el ends here
