;;; python -- init python env
;;; Commentary:
;;; Code:
;;(setq python-indent 4)
;;(setq py-indent-offset 2)
;;(require 'python)

(use-package pyenv-mode)
(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("ipython" . python-mode))

(use-package jedi
  :hook
  (python-mode-hook . jedi:setup)
  (python-mode-hook . jedi:ac-setup)
  (python-mode-hook . jedi:dot-complete)
  :bind
  (:map python-mode-map
        ("C-c j" . "jedi:complete")))

(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-python-command-args
      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
;; switch to the interpreter after executing code
(setq python-shell-interpreter "ipython"
       python-shell-interpreter-args "-i")

(add-hook 'python-mode-hook 'flymake-mode)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(add-hook 'python-mode-hook (lambda ()
                              (setq indent-tabs-mode nil)
                              (setq tab-width 4)
                              (setq python-indent 4)
                              (setq c-basic-offset 4)
                              (setq c-basic-indent 4)
                              (autoload 'pylookup-lookup "pylookup")
                              (autoload 'pylookup-update "pylookup")))


(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

;; don't split windows
(setq py-split-windows-on-execute-p nil)

;; try to automagically figure out indentation
(setq py-smart-indentation t)
(setq py-force-py-shell-name-p t)

(use-package pip-requirements)
(provide 'init-python)

;;; init-python.el ends here
