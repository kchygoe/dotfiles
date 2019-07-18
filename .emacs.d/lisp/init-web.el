;;; web -- web-mode setup
;;; Commentary:
;;; Code:

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  :hook
  (web-mode-hook . (lambda ()
                     (when (string-equal "tsx" (file-name-extension buffer-file-name))
                       (typescript-mode))))
  :config
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2)
  (setq indent-tabs-mode t)
  (setq tab-width 2))

(provide 'init-web)

;;; init-web.el ends here
