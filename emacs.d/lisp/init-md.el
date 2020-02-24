;;; Markdown -- markdown settings
;;; Commentary:
;;; Code:
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :config
  'whitespace-cleanup-mode
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))

(provide 'init-md)
;;; init-md.el ends here
