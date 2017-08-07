;;; python -- init python env
;;; Code:
;;(setq python-indent 4)
;;(setq py-indent-offset 2)
;;(require 'python)

(use-package pyenv-mode)
n(use-package jedi)
(use-package python-mode)
(add-to-list 'auto-mode-alist '("\.py\'" . python-mode))
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
                              (autoload 'pylookup-update "pylookup")
                              (guess-style-guess-tab-width) ))

(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-python-command-args
      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
                                        ; switch to the interpreter after executing code

(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

;; don't split windows
(setq py-split-windows-on-execute-p nil)

;; try to automagically figure out indentation
(setq py-smart-indentation t)
(setq py-force-py-shell-name-p t)

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook 'jedi:dot-complete)
(add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)
(add-hook 'python-mode-hook (lambda()
                              (when indent-tabs-mode
                                (guess-style-guess-tab-width))))
(define-key python-mode-map (kbd "C-c j") 'jedi:complete)

(use-package pip-requirements)

(provide 'init-python)

;;; init-python.el ends here
