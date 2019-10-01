;;; init.el --- Global Initialization for local emacs
;; Author: Koichi Yoshigoe <koichi.yoshigoe@gmail.com>
;; Ver. 0.1.1
;;; Commentary:
;; Emacs is fun

;;; Code:
;;; profile-start
;; (profiler-start 'cpu)

;; Debug
(setq debug-on-error t)

;; for 26.2 tls issue
;;To make gnu elpa available
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Package.el
(setq package-archives '(
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ))
(package-initialize)

;;; mylisps/site-lisp
(add-to-list 'load-path
             "~/.emacs.d/lisp/"
             "~/.emacs.d/site-lisp/")
;(setq init-loader-byte-compile t)

;; Basics
(setq inhibit-startup-message t)
(setq delete-auto-save-files t)
(setq frame-title-format "%f")
(setq c-auto-newline t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(fset 'yes-or-no-p 'y-or-n-p)
(setq vc-follow-symlinks t)

(setq mouse-wheel-follow-mouse t)
(setq tab-width 2)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
;;(setq-default 'truncate-lines nil)
(global-auto-revert-mode 1)
;; (defvaralias 'c-basic-offsett 'tab-width)
;; (defvaralias 'cperl-indent-level 'tab-width)

(put 'upcase-region 'disabled nil)
(show-paren-mode 1)
(line-number-mode t)
(column-number-mode t)
(add-to-list 'global-mode-string '(" %i"))

;; menu-bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; desktop file
(desktop-save-mode 1)
(setq desktop-save t)
(setq desktop-restore-eager 10)
(setq desktop-path '("~/.emacs.d/desktop/"))

;; Custom variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Tramp
(setq tramp-terminal-type "tramp") ;; default: dumb
(setq tramp-default-method "ssh")

;; OS: macos
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
  ;; Cask on mac brew
  (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
  ;; (setq mac-right-option-modifier 'meta)
  (setq mac-right-option-modifier 'super)
  (setq mac-right-command-modifier 'meta)
  (setq ns-auto-hide-menu-bar t)
  (setq ns-use-proxy-icon nil)
  )

;; OS: Linux
(when (eq system-type 'gnu/linux)
  ;; Cask on linux
  (require 'cask)
  )

;; Cask
(cask-initialize)

;;; Visualize boot from here
(require 'initchart)
(initchart-record-execution-time-of load file)
(initchart-record-execution-time-of require feature)

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
  :no-require t
  :config (pallet-mode t))

;; bind-key
(bind-key "<C-return>" 'other-window)
(bind-key "C-c i" 'indent-region)
(bind-key "C-c C-i" 'dabbrev-expand)
(bind-key "C-c ;" 'comment-region)
(bind-key "C-c :" 'uncomment-region)
(bind-key "C-c s" 'query-replace)
(bind-key "C-u" 'scroll-down)
(bind-key "C-h" 'delete-backward-char)
(bind-key "M-?" 'help-for-help)
(bind-key "M-n" 'goto-line)

;; linum
;; (global-linum-mode t)
;; (setq linum-delay t)
;; (defadvice linum-schedule (around my-linum-schedule () activate)
;;   (run-with-idle-timer 0.2 nil #'linum-update-current))

;; for gui app
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GCAL_CLIENT_ID")
  (exec-path-from-shell-copy-env "GCAL_CLIENT_SECRET")
  (exec-path-from-shell-copy-env "GCAL_EMAIL_PRIVATE")
  (exec-path-from-shell-copy-env "GCAL_EMAIL_WORK")
  (exec-path-from-shell-copy-env "ASANA_TOKEN")
  (exec-path-from-shell-copy-env "GITHUB_TOKEN") )

;; helm
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-c r" . helm-recentf)
         ("C-c C-h i" . helm-imenu)
         ("C-c C-h k" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-c h" . helm-command-prefix)
         :map helm-map
         ("C-h" . delete-backward-char)
         ("TAB" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         ("C-p" . helm-previous-line)
         ("C-n" .  helm-next-line)
         ("C-M-n" . helm-next-source)
         ("C-M-p" . helm-previous-source))
  :config
  (helm-mode +1)
  (helm-descbinds-mode +1)
  ;; (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  ;; (kill-new (buffer-substring (point) (field-end))))
   :custom
   (helm-delete-minibuffer-contents-from-point t)
   )

(use-package helm-config)
(use-package helm-swoop
  :after (helm helm-config)
  :custom
  (helm-multi-swoop-edit-save t)
  (helm-swoop-split-with-multiple-windows nil)
  (helm-swoop-split-direction 'split-window-vertically)
  (helm-swoop-speed-or-color nil)
  (helm-swoop-move-to-line-cycle t)
  (helm-swoop-use-line-number-face t)
  (helm-swoop-use-fuzzy-match t)
  :bind
  (("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)))

;; projectile
(use-package projectile
  :after helm
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(use-package helm-files
  :no-require t
  :after helm
  :config
  (remove-hook 'post-self-insert-hook 'helm-find-files--reset-level-tree)
  :bind (
         :map helm-find-files-map
              ("C-M-u" . helm-find-files-down-one-level)
              ("C-c C-o" . helm-ff-run-switch-other-window)))

(use-package helm-gtags
  :after helm
  :no-require t
  :bind (
         :map helm-gtags-mode-map
              ("M-t" . helm-gtags-find-tag)
              ("M-r" . helm-gtags-find-rtag)
              ("M-s" . helm-gtags-find-symbol)
              ("C-c >" . helm-gtags-next-history)
              ("C-c <" . helm-gtags-previous-history)
              ("C-t" . helm-gtags-pop-stack)))

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
(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-tomorrow-night t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))
;;(load-theme 'ample-zen t)
;;(load-theme 'zenburn t)
(use-package all-the-icons
  :no-require)


;; yasnippet
(use-package yasnippet
  :no-require t
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

;; company
(use-package company
  :bind (
         :map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-h" . nil)
              ("C-s" . company-filter-candidates)
              ("C-i" . company-complete-selection)
              ("<tab>" . company-complete-common-or-cycle)
              ("M-d" . company-show-doc-buffer)
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
                      :background "gray40")
  ;; Add yasnippet support for all company backends.
  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend (append (if (consp backend) backend (list backend))
                        '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

;; lsp-mode
(use-package lsp-mode
  :no-require t
  :commands lsp
  :custom
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (create-lockfiles nil)
  :hook
  (prog-major-mode . lsp-prog-major-mode-enable))

(use-package lsp-ui
  :after lsp-mode
  :custom (scroll-margin 0)
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :after (:all lsp-mode company yasnippet)
  )
  ;; :defines company-backends
  ;; :functions company-backend-with-yas
  ;; :init (cl-pushnew (company-backend-with-yas 'company-lsp) company-backends))

;; flycheck
(use-package flycheck
  :no-require t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-display-errors-delay 0.5)
  (flycheck-display-errors-function nil)
  (flycheck-idle-change-delay 2.0)
  :config
  (global-flycheck-mode t))

(use-package flycheck-popup-tip
  :after flycheck
  :hook (flycheck-mode-hook . flycheck-popup-tip-mode)
  )

;; magit
(use-package magit
  :no-require t
  :bind ("C-x g" . magit-status)
  :config
  (add-hook 'git-commit-mode-hook 'goto-address-mode)
  )

;;(use-package fullframe)

;; git-gutter+
(use-package git-gutter+
  :no-require t
  :bind
  ("C-x C-v" . git-gutter+-show-hunk-inline-at-point)
  :config
  (global-git-gutter+-mode))

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
  :no-require t
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :config (which-key-mode t))

;; hide-mode-line
(use-package hide-mode-line
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode)
   . hide-mode-line-mode))

;; doom-modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-github nil)
  (doom-modeline-minor-modes (featurep 'minions))
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-height 1)
  (doom-modeline-lsp t)  )

;; PowerLine
;; (use-package powerline
;;   :config
;;   (powerline-center-theme)
;;   (setq powerline-arrow-shape 'arrow))

;; dumb-jump
(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'helm))

;; neotree
(use-package neotree
  :no-require t
  :bind
  ("C-o" . neotree-toggle)
  :config
  (setq neo-theme 'icons)
  (setq neo-persist-show t)
  (setq neo-smart-open t)
  (setq-default neo-show-hidden-files t))

;; tabbar
;; (tabbar-mode 1)

;; popwin
;; (require 'popwin)
;; (setq display-buffer-alist 'popwin:display-buffer)

;; dashboard
;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook))

;; tree-undo
;; (use-package undo-tree
;;   :bind ("C-M-z" . undo-tree-redo)
;;   :config
;;   (global-undo-tree-mode))

;; expand-region
(use-package expand-region
  :no-require t
  :bind ("C-=" . er/expand-region))

;; golden ratio
(use-package golden-ratio
  :no-require t
  :config
  (golden-ratio-mode 1))

;; rainbow
(use-package rainbow-identifiers
  :no-require t
  :hook
  (prog-mode-hook . rainbow-identifiers-mode))

(use-package rainbow-delimiters
  :no-require t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

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

;; col-highlight
;; (use-package col-highlight)
;; (column-highlight-mode 1)

;; smartparens
(use-package smartparens
  :config
  (smartparens-global-mode))

;; mmm-mode
;; (use-package mmm-mode
;;   :config
;;   (setq mmm-global-mode 'maybe))

;; smart-newline
(use-package smart-newline
  :bind
  ("C-m" . smart-newline))

;; server-mode
(use-package server)
(unless (server-running-p)
  (server-start))

;; async
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; guess-style
(use-package guess-style
  :config
  (autoload 'guess-style-set-variable "guess-style" nil t)
  (autoload 'guess-style-guess-variable "guess-style")
  (autoload 'guess-style-guess-all "guess-style" nil t)
  (global-guess-style-info-mode 1))

;;
;; Org-mode
;;
(use-package org
  :mode ("\\.org$" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (setq org-directory (expand-file-name "~/GatsbyDrive/org"))
  (setq org-default-notes-file (concat org-directory "/note.org"))
  (setq org-agenda-files '("~/GatsbyDrive/org/agenda.org"
                           "~/GatsbyDrive/org/note.org"
                           "~/GatsbyDrive/org/journal.org"
                           "~/GatsbyDrive/org/private.org"
                           "~/GatsbyDrive/org/idea.org"
                           "~/GatsbyDrive/org/gcal.org"
                           "~/GatsbyDrive/org/gcal-work.org"))
  (setq org-log-done 'time)
  (setq org-startup-truncated nil)
  ;;(setq org-export-coding-system 'utf-8)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c@)")))
  (setq org-capture-templates
        '(("a" "Appointment" entry (file  "~/GatsbyDrive/org/gcal.org" )
            "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
          ("l" "Link" entry (file+headline "~/GatsbyDrive/org/links.org" "Links")
           "* %? %^L %^g \n%T" :prepend t)
          ("i" "Idea" entry (file+headline "~/GatsbyDrive/org/idea.org" "Idea Topics:")
           "* %?\n%T" :prepend t)
          ("t" "To Do Item" entry (file+headline "~/GatsbyDrive/org/note.org" "TODO")
           "* TODO %?\n%u" :prepend t)
          ("n" "Note" entry (file+headline "~/GatsbyDrive/org/note.org" "Note space")
           "* %?\n%u" :prepend t)
          ("j" "Journal" entry (file+datetree "~/Dropbox/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((awk . t)
                                 (emacs-lisp . t)
                                 (js . t)
                                 (makefile . t)
                                 (org . t)
                                 (python . t)
                                 (shell . t)
                                 (go . t)
                                 ))
  (add-hook 'org-agenda-mode-hook
          (lambda ()
            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
            (auto-save-mode))) )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; org-gcal
(use-package org-gcal
  :ensure t
  :hook
  (org-agenda-mode-hook . org-gcal-sync)
  :config
  (defvar gcal-email-private (getenv "GCAL_EMAIL_PRIVATE"))
  (defvar gcal-email-work (getenv "GCAL_EMAIL_WORK"))
  (setq org-gcal-client-id (getenv "GCAL_CLIENT_ID")
        org-gcal-client-secret (getenv "GCAL_CLIENT_SECRET")
        org-gcal-file-alist '(("gatsby.gatsby.gatsby@gmail.com" . "~/GatsbyDrive/org/gcal.org")
                              ("yoshigoe@leapmind.io" . "~/GatsbyDrive/org/gcal-work.org"))))

;; asana
(use-package asana
  :hook
  (org-mode-hook . asana-mode) ;; USE ASANA_TOKEN in env
  )

;; bazel
(use-package bazel-mode
  :ensure t
  :mode ("\\.bzl\\'" "BUILD\\'" "WORKSPACE\\'")
  :config
  (defun find-parent-directory-with-file(name)
    (projectile-locate-dominating-file (file-truename (buffer-file-name)) name))

  (defun bazel-build-current ()
    "Build & test in the first parent directory containing BUILD."
    (interactive)
    (let ((default-directory (find-parent-directory-with-file  "BUILD")))
      (if default-directory
          (compile "bazel test ...  --test_output=all --test_arg=--log_level=message")
        (error "BUILD file not found in the parent directories"))))

  (defun bazel-build-workspace ()
    "Build & test in the first parent directory containing WORKSPACE."
    (interactive)
    (let ((default-directory (find-parent-directory-with-file  "WORKSPACE")))
      (if default-directory
          (compile "bazel test ...")
        (error "WORKSPACE file not found in the parent directories"))))
  ;; (define-key c++-mode-map (kbd "C-c n") 'bazel-build-current)
  ;; (define-key c++-mode-map (kbd "C-c b") 'bazel-build-workspace)
  )

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
  (global-whitespace-mode 1)
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
  )

(provide 'init)

;;; profiler
;; (profiler-stop)
;; (profiler-report)

;;;
;;; init.el ends here
;;;

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
