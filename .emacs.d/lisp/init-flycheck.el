;;; init-flycheck.el -- set flycheck
;;; Commentary:
;;; Code:

(use-package flycheck
  :config
  (global-flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (custom-set-variables
   '(flycheck-display-errors-delay 0.5)
   '(flycheck-idle-change-delay 1.0)
   '(flycheck-display-errors-function nil)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
