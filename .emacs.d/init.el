;;; init.el --- Global Initialization for local emacs
;; Author: Koichi Yoshigoe <koichi.yoshigoe@gmail.com>
;; Ver. 0.1.1
;;; Commentary:
;;; Code:

;;; profile-start
;; (profiler-start 'cpu)

;;; mylisps/site-lisp
(add-to-list 'load-path
             "~/.emacs.d/lisp/"
             "~/.emacs.d/site-lisp/")
(let ((default-directory  "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
;(setq init-loader-byte-compile t)

;;; Visualize boot
(require 'initchart)
(initchart-record-execution-time-of load file)
(initchart-record-execution-time-of require feature)

;; Basics
(setq backup-inhibited t)
(setq inhibit-startup-message t)
(setq delete-auto-save-files t)
; (setq auto-save-default nil)
; (setq make-backup-files nil)
(setq frame-title-format "%f")
(fset 'yes-or-no-p 'y-or-n-p)

(line-number-mode t)
(column-number-mode t)
(add-to-list 'global-mode-string '(" %i"))

(when (eq system-type 'darwin)
  ;; Mac-only
  ;; Command key as Meta key, Option key untouched
  ;; http://www.emacswiki.org/emacs/MetaKeyProblems#toc15
  ;; http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
  ;; left command
  ;; (setq mac-command-modifier 'meta)
  ;; ;; left option
  ;; (Setq Mac-option-modifier 'alt)
  ;;
  ;; right command
  ;; (setq mac-right-command-modifier 'super)
  ;; right option
  (setq mac-right-option-modifier 'super))

;;;全自動インデント
(setq c-auto-newline t)
;;;[TAB］キーでインデント実施
;; (setq c-tab-always-indent t)

;; menu-bar
(menu-bar-mode -1)

;; desktop file
(desktop-save-mode 1)
(setq desktop-save t)
(setq desktop-restore-eager 10)
(setq desktop-path '("~/.emacs.d/desktop"))
(global-auto-revert-mode 1)

;; Debug
(setq debug-on-error t)

;; cask & pallet
;;; Cask on mac brew
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
;;; Cask on ubuntu
;(require 'cask "")

(cask-initialize)
;; use-package, bind-key
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args))
  (defmacro bind-key (&rest args)) )

;; pallet for cask
(use-package pallet
  :defer t
  :config
  (pallet-mode t))

;; packages elpa & melpa
;(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/") t)

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

;; linum
;; (global-linum-mode t)
;; (setq linum-delay t)
;; (defadvice linum-schedule (around my-linum-schedule () activate)
;;   (run-with-idle-timer 0.2 nil #'linum-update-current))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-c h" . helm-mini)
         ("C-c r" . helm-recentf)
         ("C-c C-h i" . helm-imenu)
         ("C-c C-h k" . helm-show-kill-ring)
         :map helm-map
         ("C-h" . delete-backward-char)
         ("TAB" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :config
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;; (define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)
  ;; (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  ;; (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; (define-key helm-read-file-map (kbd "C-z") 'helm-select-action)
  ;; (define-key helm-find-files-map (kbd "C-z") 'helm-select-action)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (helm-mode 1)
  (helm-descbinds-install)
  (helm-descbinds-mode +1)
  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  ;; (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  ;; (kill-new (buffer-substring (point) (field-end))))
  )
(use-package helm-config)
(use-package helm-swoop
  :config
  (global-set-key (kbd "M-i") 'helm-swoop)
  (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
  (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-split-with-multiple-windows nil)
  (setq helm-swoop-split-direction 'split-window-vertically)
  (setq helm-swoop-speed-or-color nil)
  (setq helm-swoop-move-to-line-cycle t)
  (setq helm-swoop-use-line-number-face t)
  (setq helm-swoop-use-fuzzy-match t))

(custom-set-variables
 '(helm-input-idle-delay 0)
 '(helm-exit-idle-delay 0)
 '(helm-candidate-number-limit 500)
 '(helm-ag-insert-at-point 'symbol)
 '(helm-find-files-doc-header "")
 '(helm-command-prefix-key nil)
 '(helm-gtags-pulse-at-cursor nil) ;gtags
 )

(with-eval-after-load 'helm
  (helm-descbinds-mode)
  (define-key helm-map (kbd "C-p")   #'helm-previous-line)
  (define-key helm-map (kbd "C-n")   #'helm-next-line)
  (define-key helm-map (kbd "C-M-n") #'helm-next-source)
  (define-key helm-map (kbd "C-M-p") #'helm-previous-source))

(with-eval-after-load 'helm-files
  (remove-hook 'post-self-insert-hook 'helm-find-files--reset-level-tree)
  (define-key helm-find-files-map (kbd "C-M-u") #'helm-find-files-down-one-level)
  (define-key helm-find-files-map (kbd "C-c C-o") #'helm-ff-run-switch-other-window))


(with-eval-after-load 'helm-gtags
  (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
  (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-t") 'helm-gtags-pop-stack))

;;; Enable helm-gtags-mode
;; (dolist (hook '(c-mode-common-hook
;;                 java-mode-hook
;;                 asm-mode-hook))
;;   (add-hook hook 'helm-gtags-mode))

;; migemo
;; (require 'helm-migemo)
;; (with-eval-after-load "helm-migemo"
;;   (defun helm-compile-source--candidates-in-buffer (source)
;;     (helm-aif (assoc 'candidates-in-buffer source)
;;         (append source
;;                 `((candidates
;;                    . ,(or (cdr it)
;;                           (lambda ()
;;                             ;; Do not use `source' because other plugins
;;                             ;; (such as helm-migemo) may change it
;;                             (helm-candidates-in-buffer (helm-get-current-source)))))
;;                   (volatile) (match identity)))
;;       source))
;;   [2015-09-06 Sun]helm-match-plugin -> helm-multi-match変更の煽りを受けて
;;   (defalias 'helm-mp-3-get-patterns 'helm-mm-3-get-patterns)
;;   (defalias 'helm-mp-3-search-base 'helm-mm-3-search-base))
;; (setq helm-use-migemo t)
;; (setq migemo-command "cmigemo")
;; (setq migemo-options '("-q" "--emacs"))
;; (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
;; (load-library "migemo")
;; (migemo-init)

;; color theme
(load-theme 'ample-zen t)
;;(load-theme 'zenburn t)

;; yasnippet
(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

;; company
(use-package company
  :bind (
         :map company-active-map
         ( "M-n" . nil)
         ( "M-p" . nil)
         ( "C-n" . company-select-next)
         ( "C-p" . company-select-previous)
         ( "C-h" . nil)
         ( "C-s" . company-filter-candidates)
         ( "C-i" . company-complete-selection)
         ( "<tab>" . company-complete-common-or-cycle)
         ( "M-d" . company-show-doc-buffer)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         :map emacs-lisp-mode-map
         ("C-M-i" . company-complete))
  :config
  (global-company-mode +1)
  (company-quickhelp-mode +1)
  (custom-set-variables '(company-idle-delay nil))
  (setq company-transformers '(company-sort-by-backend-importance))
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  ;; (setq company-dabbrev-downcase nil)
  ;; like auto-complete
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40"))

;; company with yasnippet
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; lsp-mode
(use-package lsp-mode
  :commands lsp)
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package company-lsp
  :commands company-lsp)

;; flycheck
(use-package flycheck
  :config
  (global-flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (custom-set-variables
   '(flycheck-display-errors-delay 0.5)
   '(flycheck-idle-change-delay 2.0)
   '(flycheck-display-errors-function nil)))

;; git
(use-package git-blamed)
(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package git-timemachine)
(use-package git-commit)
(use-package magit)

;;(use-package fullframe)
(add-hook 'git-commit-mode-hook 'goto-address-mode)

;; git-gutter+
(use-package git-gutter+
  :bind
  ("C-x C-v" . git-gutter+-show-hunk-inline-at-point)
  :config
  (global-git-gutter+-mode 1))

;; hi-line
(use-package hl-line
  :config
  (defun global-hl-line-timer-function ()
    (global-hl-line-unhighlight-all)
    (let ((global-hl-line-mode t))
      (global-hl-line-highlight)))
  (setq global-hl-line-timer
        (run-with-idle-timer 0.3 t 'global-hl-line-timer-function)))
;; (cancel-timer global-hl-line-timer)

;; indent-guide
(use-package indent-guide
  :config
  (indent-guide-global-mode)
  (setq indent-guide-delay 0.2))

;; which-key
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;; hide-mode-line
(use-package hide-mode-line
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode)
   . hide-mode-line-mode))

;; neotree
(use-package neotree
  :config
  (setq neo-theme 'icon)
  (setq neo-persist-show t)
  (setq neo-smart-open t)
  (global-set-key "\C-o" 'neotree-toggle))

;; ido-mode
;; (use-package ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (setq ido-enable-flex-matching t)

;; tabbar
;; (tabbar-mode 1)

;; popwin
;; (require 'popwin)
;; (setq display-buffer-alist 'popwin:display-buffer)

;; dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; tree-undo
(use-package undo-tree
  :bind ("C-M-z" . undo-tree-redo)
  :config
  (global-undo-tree-mode))

;; expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; PowerLine
(use-package powerline
  :config
  (powerline-center-theme)
  (setq powerline-arrow-shape 'arrow))

;; golden ratio
(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode 1))

;; smooth-scroll
;; (use-package 'smooth-scroll)
;; (smooth-scroll-mode t)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; (setq mouse-wheel-progressive-speed nil) don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) scroll window under mouse
;; (setq scroll-step 1)
;; (setq scroll-conservatively 10000)
;; (defcustom smooth-scroll/hscroll-step-size 8)
;; (defcustom smooth-scroll/vscroll-step-size 8)

;; mutiple-cursors
(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  ("M-<mouse-1>" . mc/add-cursor-on-click)
  :config
  (global-unset-key (kbd "M-<down-mouse-1>")))

;; mutiple-cursors
(use-package multiple-cursors
  :bind (
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  ("M-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (global-unset-key (kbd "M-<down-mouse-1>")))

;; col-highlight
;; (use-package col-highlight)
;; (column-highlight-mode 1)

;; smartparens
(use-package smartparens
  :config
  (smartparens-global-mode))

;; mmm-mode
(use-package mmm-auto
  :config
  (setq mmm-global-mode 'maybe))

;; smart-newline
(use-package smart-newline
  :bind
  ("C-m" . smart-newline))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
;;(use-package server)
;; (unless (server-running-p)
;;   (server-start))

;; async
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; org-mode
(use-package org
  :mode (("\\.org$" . org-mode))
  :init
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  :config
  (setq org-directory (expand-file-name "~/GatsbyDrive/org"))
  (setq org-default-notes-file (concat org-directory "/note.org"))
  (setq org-agenda-files '("~/GatsbyDrive/org", "~/GatsbyDrive/org/agenda"))
  )
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(setq org-log-done 'time)
(setq org-todo-keywords
      '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c@)")))

(use-package guess-style
  :load-path "~/.emacs.d/site-lisp/guess-style/"
  :config
  (autoload 'guess-style-set-variable "guess-style" nil t)
  (autoload 'guess-style-guess-variable "guess-style")
  (autoload 'guess-style-guess-all "guess-style" nil t)
  (global-guess-style-info-mode 1))

;; whitespace
(use-package whitespace
  :config
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
  (setq whitespace-action '(auto-cleanup))
  (global-whitespace-mode 1))

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
		    :foreground "DarkGreen"
		    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
		    :background my/bg-color)

;;
;; load all own lisps
;;
(require 'init-golang)
;; (require 'init-json)
;; (require 'init-md)
(require 'init-python)
(require 'init-tide)
(require 'init-web)


(provide 'init)
;;;
;;; init.el ends here
;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ag-insert-at-point (quote symbol))
 '(helm-candidate-number-limit 500)
 '(helm-command-prefix-key nil)
 '(helm-exit-idle-delay 0)
 '(helm-find-files-doc-header "" t)
 '(helm-gtags-pulse-at-cursor nil)
 '(helm-input-idle-delay 0)
 '(helm-mode-fuzzy-match t)
 '(which-key-mode t))

;;; profiler
;; (profiler-stop)
;; (profiler-report)
