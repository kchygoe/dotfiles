;;; init.el --- Global Initialization for local emacs
;; Author: Koichi Yoshigoe <koichi.yoshigoe@gmail.com>
;; Ver. 0.1.1
;;; Commentary:

;;; profile-start
;; (profiler-start 'cpu)

;;; My lisps
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq init-loader-byte-compile t)

;;; Visualize boot
(require 'initchart)
(initchart-record-execution-time-of load file)
(initchart-record-execution-time-of require feature)
;;; end of visualize

;;; Code:
;; Basics
(setq backup-inhibited t)
(setq inhibit-startup-message t)
(setq delete-auto-save-files t)
; (setq auto-save-default nil)
; (setq make-backup-files nil)
(setq frame-title-format "%f")
(fset 'yes-or-no-p 'y-or-n-p)
(desktop-save-mode 1)
(setq desktop-save t)
(setq desktop-restore-eager 10)
(global-auto-revert-mode 1)

;; Debug
(setq debug-on-error t)

;; cask & pallet
;;; Cask on mac bre
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
;;; Cask on ubuntu
;(require 'cask "")

(cask-initialize)
;; use-package, bind-key
(require 'use-package)
(require 'bind-key)
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args))
  (defmacro bind-key (&rest args)) )

;; pallet for cask
;;(require 'pallet)
(use-package pallet)
(pallet-mode t)

;; packages elpa & melpa
;(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("elpy" . "https://jorgenschaefer.github.io/packages/"))
;(package-initialize)
; (elpy-enable)


;; bind-key
;; (bind-key "<C-return>" 'other-window)
(bind-key "C-c i" 'indent-region)
(bind-key "C-c C-i" 'dabbrev-expand)
(bind-key "C-c ;" 'comment-region)
(bind-key "C-c :" 'uncomment-region)
(bind-key "C-c s" 'query-replace)
(bind-key "C-u" 'scroll-down)
(bind-key "C-h" 'delete-backward-char)
(bind-key "M-?" 'help-for-help)
(bind-key "M-n" 'goto-line)

(setq mouse-wheel-follow-mouse t)
(setq tab-width 2)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
;; (defvaralias 'c-basic-offsett 'tab-width)
;; (defvaralias 'cperl-indent-level 'tab-width)

;;(setq kill-whole-line t)
(put 'upcase-region 'disabled nil)
;;(mouse-wheel-mode t)
;;(auto-compression-mode t)
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
(add-to-list 'global-mode-string '(" %i"))
;;;全自動インデント
(setq c-auto-newline t)
;;;[TAB］キーでインデント実施
;; (setq c-tab-always-indent t)

;; menu-bar
(menu-bar-mode -1)

;; yasnippet
(use-package yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; company
(use-package company)
(global-company-mode +1)
(setq company-transformers '(company-sort-by-backend-importance))
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 3)
(setq company-selection-wrap-around t)
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)
(define-key company-active-map (kbd "C-i") 'company-complete-selection)
(define-key company-active-map [tab] 'company-complete-selection)
(define-key company-active-map (kbd "C-f") 'company-complete-selection)
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

;; yasnippetとの連携
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; helm
(use-package helm)
(use-package helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)
;(define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)
;(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
;(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
;(define-key helm-read-file-map (kbd "C-z") 'helm-select-action)
;(define-key helm-find-files-map (kbd "C-z") 'helm-select-action)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(helm-mode 1)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(use-package server)
;; (unless (server-running-p)
;;   (server-start))

;; indent-guide
(use-package indent-guide)
(indent-guide-global-mode)
(setq indent-guide-delay 0.2)

;; async
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; flycheck
(use-package flycheck)
(global-flycheck-mode 1)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; lsp-mode
(use-package lsp-mode
  :commands lsp)
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package company-lsp
  :commands comapny-lsp)

;; neotree
(setq neo-theme 'icon)
(setq neo-persist-show t)
(setq neo-smart-open t)
(global-set-key "\C-o" 'neotree-toggle)

;; hide-mode-line
(use-package hide-mode-line
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode)
   . hide-mode-line-mode))

;; which-key
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;; pyenv
(use-package pyenv-mode
  :hook
  (pyenv-mode))

;;;
;;; init.el ends here
;;;

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
 '(helm-mode-fuzzy-match t)
 '(ivy-mode t)
 '(package-selected-packages
   (quote
    (pyenv-mode-auto company-lsp lsp-ui lsp-mode helm-company helm-go-package helm-google helm-grepint company-c-headers init-loader which-key amx zenburn-theme yascroll wgrep-helm vagrant-tramp use-package tss tree-mode tide terraform-mode ssh-config-mode smartrep smart-newline sequential-command rainbow-delimiters qiita python-mode py-yapf projectile prodigy powerline popwin php-mode pbcopy pallet nyan-mode nginx-mode neotree multiple-cursors mmm-mode markdown-preview-mode markdown-preview-eww markdown-mode+ kubernetes-evil k8s-mode jinja2-mode jedi jade-mode indent-guide idle-highlight-mode htmlize highlight-symbol helm-themes helm-migemo helm-ls-git helm-gtags helm-git-grep helm-git helm-ghq helm-describe-modes helm-descbinds graphene golden-ratio go-autocomplete gitignore-mode git-gutter git gist ghub flymake-yaml flymake-python-pyflakes flymake-json flycheck-cask expand-region elpy drag-stuff dockerfile-mode docker dash-functional company-quickhelp company-jedi company-go coffee-mode auto-yasnippet apache-mode anzu ansible android-mode ample-zen-theme)))
 '(pyenv-mode t)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(which-key-mode t))

(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; profiler
;; (profiler-stop)
;; (profiler-report)
