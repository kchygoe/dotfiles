;;; tide -- tide-mode config
;;; Commentary:
;;; Code:

(use-package typescript-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (js2-mode-hook . tide-setup)
         (before-save . tide-format-before-save)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

(provide 'init-tide)

;;; init-tide.el ends here
