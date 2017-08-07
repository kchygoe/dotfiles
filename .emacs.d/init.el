;;; init.el --- Global Initialization for local emacs
;; Author: Koichi Yoshigoe <koichi.yoshigoe@gmail.com>
;; Ver. 0.1.0

;;; Visualize boot
;(add-to-list 'load-path "~/.emacs.d/lisp/")
;(require 'initchart)
;(initchart-record-execution-time-of load file)
;(initchart-record-execution-time-of require feature)
;;; end of visualize

;;; Code:
;; Basics
(setq backup-inhibited t)
(setq inhibit-startup-message t)
(setq delete-auto-save-files t)
(desktop-save-mode 1)

;; Debug
(setq debug-on-error t)

;; cask & pallet
;; cask
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
;; pallet
(require 'pallet)

;; use-package, bind-key
(require 'use-package)
(require 'bind-key)
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args))
  (defmacro bind-key (&rest args)) )

;; others
(add-to-list 'load-path "~/.emacs.d/elpa/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; packages elpa & melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(package-initialize)
(elpy-enable)

;; bind-key
;; (bind-key "<C-return>" 'other-window)
(bind-key "C-c i" 'indent-region)
(bind-key "C-c C-i" 'dabbrev-expand)
(bind-key "C-c /" 'comment-region)
(bind-key "C-c ;" 'uncomment-region)
(bind-key "C-c s" 'query-replace)
(bind-key "C-u" 'scroll-down)
(bind-key "C-h" 'delete-backward-char)
(bind-key "M-?" 'help-for-help)
(bind-key "M-n" 'goto-line)

;(setq mouse-wheel-follow-mouse t)
(setq tab-width 2)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
;; (defvaralias 'c-basic-offsett 'tab-width)
;; (defvaralias 'cperl-indent-level 'tab-width)

;;(setq kill-whole-line t)
(put 'upcase-region 'disabled nil)
;;(mouse-wheel-mode t)
(auto-compression-mode t)
(show-paren-mode 1)

;; hi-line
(require 'hl-line)
(defun global-hl-line-timer-function ()
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq global-hl-line-timer
      (run-with-idle-timer 0.3 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)

;; linum
;; (global-linum-mode t)
;; (setq linum-delay t)
;; (defadvice linum-schedule (around my-linum-schedule () activate)
;;   (run-with-idle-timer 0.2 nil #'linum-update-current))

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
;;(load-theme 'zenburn t)

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
;;     "~/.rbenv/versions/2.1.2/bin"
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

;; menu-bar
(menu-bar-mode -1)

;; auto-install
(use-package auto-install)
(auto-install-compatibility-setup)

;; yasnippet
(use-package yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
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
(use-package indent-guide)
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

;; async
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; flycheck
(use-package flycheck)
(flycheck-mode 1)
(global-flycheck-mode 1)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay t)
 '(helm-ag-insert-at-point (quote symbol))
 '(helm-candidate-number-limit 500)
 '(helm-command-prefix-key nil)
 '(helm-exit-idle-delay 0)
 '(helm-find-files-doc-header "" t)
 '(helm-gtags-pulse-at-cursor nil)
 '(helm-input-idle-delay 0)
 '(package-selected-packages
   (quote
    (kotlin-mode nginx-mode zenburn-theme yascroll yaml-mode wgrep vagrant-tramp use-package tss tree-mode tide terraform-mode ssh-config-mode smartrep smart-newline sequential-command rainbow-delimiters qiita python-mode py-yapf projectile prodigy powerline popwin php-mode pbcopy pallet nyan-mode neotree multiple-cursors mmm-mode markdown-preview-eww magit jinja2-mode jedi jade-mode idle-highlight-mode htmlize highlight-symbol helm-themes helm-migemo helm-ls-git helm-gtags helm-git-grep helm-git helm-ghq helm-describe-modes helm-descbinds graphene golden-ratio go-mode gitignore-mode git-modes git-gutter git gist flymake-yaml flymake-python-pyflakes flymake-json flycheck-cask expand-region drag-stuff dockerfile-mode docker dash-functional company-quickhelp company-jedi column-marker col-highlight coffee-mode auto-yasnippet auto-install anzu ansible android-mode anaconda-mode ample-zen-theme)))
 '(pyenv-mode t)
 '(safe-local-variable-values (quote ((encoding . utf-8)))))

(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
