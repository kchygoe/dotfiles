;;; golang -- init golang enva
;;; Code:
;;; Commentary:

(add-to-list 'exec-path (expand-file-name "~/.go/bin"))
(use-package go-mode
  :custom
  (setq tab-width 2)
  (setq completion-ignore-case t)
  :bind
  ("M-." . godef-jump)
  ("C-M-i" . company-complete)
  :hook
  (before-save-hook . gofmt-before-save)
  )
(use-package company-go)
(use-package go-autocomplete)
;(use-package go-flymake)
;; (use-package lsp-go
;;   :after (lsp-mode)
;;   ;; :custom (lsp-go-language-server-flags '(
;;   ;;   "-gocodecompletion"
;;   ;;   "-lint-tool=golint"))
;;   :commands lsp-go-enable
;;   :hook (go-mode . lsp-go-enable)
;; )

;; (add-hook 'go-mode-hook (lambda()
;;                           (setq company-transformers '(company-sort-by-backend-importance))
;;                           (setq company-minimum-prefix-length 3)
;;                           (setq company-dabbrev-downcase nil)
;;
;;                           (define-key company-active-map (kbd "C-n") 'company-select-next)
;;                           (define-key company-active-map (kbd "C-p") 'company-select-previous)
;;                           (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
;;                           (setq company-idle-delay 0)
;;                           (set (make-local-variable 'company-backends) '(company-go)))

(provide 'init-golang)

;;; init-golang.el ends here
