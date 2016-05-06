;; python
;;(setq python-indent 2)
;;(setq py-indent-offset 2)
;;(require 'python)
(require 'jedi)
(require 'python-mode)

(add-to-list 'auto-mode-alist '("\.py\'" . python-mode))
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

(autoload 'pylookup-lookup "pylookup")
(autoload 'pylookup-update "pylookup")
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook 'jedi:complete-on-dot)
(add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)
(add-hook 'python-mode-hook (lambda()
															(when indent-tabs-mode
																(guess-style-guess-tab-width))))
(define-key python-mode-map (kbd "C-cj") 'jedi:complete)

(require-package 'pip-requirements)
