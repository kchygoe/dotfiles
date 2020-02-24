;;; json -- for json files
;;; Code:
;;; Commentary:

(use-package json-mode
  (add-hook 'json-mode-hook
          (lambda ()
            'flymake-json-load
            'json-mode-beautify
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)
            ))
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))

(use-package flymake-json)

(provide 'init-json)

;;; init-json.el ends here
