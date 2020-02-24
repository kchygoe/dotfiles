;;; python -- init python env
;;; Commentary:
;;; Code:
;;(setq python-indent 4)
;;(setq py-indent-offset 2)
;;(require 'python)

(use-package pyenv-mode)
(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("ipython" . python-mode)
  :hook (python-mode-hook . lsp)
  )

(use-package jedi
  :hook
  (python-mode-hook . jedi:setup)
  (python-mode-hook . jedi:ac-setup)
  (python-mode-hook . jedi:dot-complete)
  (python-mode-hook . flymake-mode)
  (python-mode-hook . flymake-python-pyflakes-load)
  :bind
  (:map python-mode-map
        ("C-c j" . "jedi:complete"))
  :config
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq python-indent 4)
  (setq c-basic-offset 4)
  (setq c-basic-indent 4)
  (autoload 'pylookup-lookup "pylookup")
  (autoload 'pylookup-update "pylookup")
  (setq py-shell-switch-buffers-on-execute-p t)
  (setq py-switch-buffers-on-execute-p t)
  (setq py-python-command-args
        '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
  ;; don't split windows
  (setq py-split-windows-on-execute-p nil)
  ;; try to automagically figure out indentation
  (setq py-smart-indentation t)
  (setq py-force-py-shell-name-p t))

(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "python")
;; switch to the interpreter after executing code
;; (setq python-shell-interpreter "ipython"
;;        python-shell-interpreter-args "-i")

;; (use-package pip-requirements)
(use-package elpy
  :config
  (setq elpy-rpc-backend "jedi")
  :hook
  (elpy-mode-hook . flycheck-mode)
  )


(provide 'init-python)

;;; init-python.el ends here
