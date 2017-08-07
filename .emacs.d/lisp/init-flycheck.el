;;; init-flycheck.el -- set flycheck

;;; Code:

(global-flycheck-mode t)
(custom-set-variables
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-idle-change-delay 1.0)
 '(flycheck-display-errors-function nil))

;;; init-flycheck.el ends here
