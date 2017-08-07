;; Markdown
;;; Code:
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))
(after-load 'whitespace-cleanup-mode
            (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))

;;; init-markdown.el ends here
