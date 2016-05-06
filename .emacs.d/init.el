;; Basics
(setq backup-inhibited t)
(setq inhibit-startup-message t)
(setq delete-auto-save-files t)

;; others
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/elpa/")

;; packages melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; cask
(require 'cask)
(cask-initialize)
(package-initialize)

(require 'use-package)

(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c C-i") 'dabbrev-expand)
(global-set-key (kbd "C-c /") 'comment-region)
(global-set-key (kbd "C-c ;") 'uncomment-region)
(global-set-key (kbd "C-c s") 'query-replace)
(global-set-key (kbd "C-u") 'scroll-down)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-?") 'help-for-help)
(global-set-key (kbd "M-n") 'goto-line)
(global-linum-mode t)

(setq mouse-wheel-follow-mouse t)

(setq default-tab-width 2)
(setq tab-width 2)
;;(setq indent-tabs-mode nil)
;; (defvaralias 'c-basic-offsett 'tab-width)
;; (defvaralias 'cperl-indent-level 'tab-width)

;;(setq kill-whole-line t)
(put 'upcase-region 'disabled nil)
;;(mouse-wheel-mode t)
(auto-compression-mode t)
(show-paren-mode 1)

;; smartparens
(use-package smartparens)
(smartparens-global-mode t)

;; whitespace
(use-package whitespace)
(setq whitespace-style '(face
												 trailing
												 tabs
												 spaces
												 empty
												 space-mark
												 tab-mark
												 ))
(setq whitespace-display-mappings
			'((space-mark ?\u3000 [?\u25a1])
				;; WARNING: the mapping below has a problem.
				;; When a TAB occupies exactly one column, it will display the
				;; character ?\xBB at that column followed by a TAB which goes to
				;; the next TAB column.
				;; If this is a problem for you, please, comment the line below.
				(tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
;;(setq whitespace-space-regexp "\\(\u3000+\\)")

;; color theme
(load-theme 'ample-zen t)
;(load-theme 'zenburn t)

;; Color for White spaces
(setq whitespace-action '(auto-cleanup))
(defvar my/bg-color "#262626")
(set-face-attribute 'whitespace-trailing nil
										:background my/bg-color
										:foreground "DarkBlue"
										:underline t)
(set-face-attribute 'whitespace-tab nil
										:background my/bg-color
										:foreground "LightSkyBlue"
										:underline t)
(set-face-attribute 'whitespace-space nil
										:background my/bg-color
										:foreground "GreenYellow"
										:weight 'bold)
(set-face-attribute 'whitespace-empty nil
										:background my/bg-color)
(global-whitespace-mode 1)

;;; 補完機能
;;;(partial-completion-mode 1)
;;;(iconplete-mode1)
(line-number-mode t)
(column-number-mode t)
;;;全自動インデント
(setq c-auto-newline t)
;;;[TAB］キーでインデント実施
;; (setq c-tab-always-indent t)

;; UTF
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq coding-system-for-read 'utf-8)
;(use-package ucs-normalize)
(setq file-name-config-system 'utf-8-hfs)
(setq locale-config-system 'utf-8-hfs)
;;(set-language-environment "Japanese")

; (setq debug-on-error t)

;; evernote-mode
;;(add-to-list 'load-path "~/.emacs.d/evernote-mode/")
;;(use-package evernote-mode)
;;(use-package org-evernote)
;; (setq evernote-username "gatsby_bob") ; optional: you can use this username as d$
;; (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option
;; (global-set-key "\C-cec" 'evernote-create-note)
;; (global-set-key "\C-ceo" 'evernote-open-note)
;; (global-set-key "\C-ces" 'evernote-search-notes)
;; (global-set-key "\C-ceS" 'evernote-do-saved-search)
;; (global-set-key "\C-cew" 'evernote-write-note)
;; (global-set-key "\C-cep" 'evernote-post-region)
;; (global-set-key "\C-ceb" 'evernote-browser)
;; (dolist (dir (list
;;		"~/.rbenv/versions/2.1.2/bin"
;; ))
;; (when (and (file-exists-p dir) (not (member dir exec-path)))
;; (setenv "PATH" (concat dir ":" (getenv "PATH")))
;; (setq exec-path (append (list dir) exec-path))))
;; (custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;; '(evernote-developer-token "S=s21:U=206b30:E=14fd6e01eb2:C=1487f2ef180:P=1cd:A=en-devtoken:V=2:H=92b5761c623a64fda94c526df58d9668")

;; PowerLine
(powerline-default-theme)
(setq powerline-arrow-shape 'arrow)

;; rainbow-delimiters
;(use-package rainbow-delimiters)(
(rainbow-delimiters-mode)

;; mutiple-cursors
;(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; helm
;(use-package helm)
(global-set-key (kbd "C-c h") 'helm-mini)
(use-package helm-config)
(helm-descbinds-mode)
(when (require 'helm-config nil t)
	(helm-mode 1))
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
;; Emulate `kill-line' in helm minibuffer
(setq helm-delete-minibuffer-contents-from-point t)
(defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
	"Emulate `kill-line' in helm minibuffer"
	(kill-new (buffer-substring (point) (field-end))))
;; For find-file etc.
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

;; migemo
(use-package helm-migemo)
(with-eval-after-load "helm-migemo"
	(defun helm-compile-source--candidates-in-buffer (source)
		(helm-aif (assoc 'candidates-in-buffer source)
				(append source
								`((candidates
									 . ,(or (cdr it)
													(lambda ()
														;; Do not use `source' because other plugins
														;; (such as helm-migemo) may change it
														(helm-candidates-in-buffer (helm-get-current-source)))))
									(volatile) (match identity)))
			source))
	(defalias 'helm-mp-3-get-patterns 'helm-mm-3-get-patterns)
	(defalias 'helm-mp-3-search-base 'helm-mm-3-search-base))
(setq helm-use-migemo t)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(load-library "migemo")
(migemo-init)

;; json-mode
;;(use-package json-mode)
;;(use-package flymake-json)
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'json-mode-hook 'flymake-json-load)

;; python
;;(setq python-indent 2)
;;(setq py-indent-offset 2)
(use-package jedi)
(use-package python-mode)
(add-to-list 'auto-mode-alist '("\.py$" . python-mode))
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-python-command-args
			'("--gui=wx" "--pylab=wx" "-colors" "Linux"))
;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
;; don't split windows
;; (setq py-split-windows-on-execute-p nil)
;; try to automagically figure out indentation
(setq py-smart-indentation t)
(setq py-force-py-shell-name-p t)
(autoload 'pylookup-lookup "pylookup")
(autoload 'pylookup-update "pylookup")
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook 'jedi:complete-on-dot)
(add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook 'elpy-enable)
(add-hook 'python-mode-hook (lambda()
															(when indent-tabs-mode
																(guess-style-guess-tab-width))))
(define-key python-mode-map (kbd "C-;") 'jedi:complete)

;; php
;; (setq php-mode-force-pear t)
;; (add-hook 'php-mode-hook
;; 					(lambda ()
;; 						(use-package php-completion) ))
;;						'my-fetch-php-completions))
;; (defun my-fetch-php-completions ()
;;		(if (and (boundp 'my-php-symbol-list)
;;						 my-php-symbol-list)
;;				my-php-symbol-list
;;			(message "Fetching completion list...")
;;			(with-current-buffer
;;					(url-retrieve-synchronously "http://www.php.net/manual/en/indexes.functions.php")
;;				(goto-char (point-min))
;;				(message "Collecting function names...")
;;				(setq my-php-symbol-list nil)
;;				(while (re-search-forward "<a[^>]*class=\"index\"[^>]*>\\([^<]+\\)</a>" nil t)
;;					(push (match-string-no-properties 1) my-php-symbol-list))
;;				my-php-symbol-list)))


;; ido-mode
;; (use-package ido)
;; (ido-mode 1)

;; tabbar
;; (tabbar-mode 1)

;; neotree
(use-package neotree)
(global-set-key [f8] 'neotree-toggle)



;; Markdown Mode
;; (use-package markdown-mode
;;							 :mode ("\\.md\\'" . gfm-mode))

;; ;; smart-newline
;; (use-package smart-newline
;;							 :config
;;							 (progn
;;								 (bind-key "C-m" 'smart-newline)))

;; menu-bar
(menu-bar-mode -1)

;; auto-install
(use-package auto-install)
(auto-install-compatibility-setup)

;; yasnippet
(use-package yasnippet)
(setq yas-snippet-dirs
			'("~/.emacs.d/snippet"
				))
(yas-global-mode 1)

;; paradox
;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(use-package server)
(unless (server-running-p)
		(server-start))

;; indent-guide
;; (use-package indent-guide)
;; (indent-guide-global-mode)
;; (setq indent-guide-delay 0.1)
;; (custom-set-variables
;;	;; custom-set-variables was added by Custom.
;;	;; If you edit it by hand, you could mess it up, so be careful.
;;	;; Your init file should contain only one such instance.
;;	;; If there is more than one, they won't work right.
;;	'(custom-safe-themes
;;		 (quote
;;			("3ed645b3c08080a43a2a15e5768b893c27f6a02ca3282576e3bc09f3d9fa3aaa" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" default)))

;; '(python-shell-interpreter "ipython"))

;; async
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
