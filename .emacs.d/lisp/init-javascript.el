;;; init-javascript.el --- for js
;;; Code:

(custom-set-variables
 '(js-auto-indent-flag nil))

(setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")

;; typescript
(use-package typescript-mode)
(use-package tide)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode t)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (tide-hl-identifier-mode +1)
            (company-mode +1)
            (eldoc-mode t)
            (company-mode-on)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(add-hook 'before-save-hook 'tide-format-before-save)

;; TSX
(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))


;;; init-javascript.el ends here
