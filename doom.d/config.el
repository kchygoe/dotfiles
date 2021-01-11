;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Code:
;;
;; Basic configs
;;
(setq user-full-name "Koichi Yoshigoe"
      user-mail-address "koichi.yoshigoe@gmail.com")

;; doom basics
(setq doom-font (font-spec :family "Hack Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font Mono" :size 14)
      doom-serif-font (font-spec :family "UbuntuMono Nerd Font" :weight 'Regular)
      doom-theme 'doom-dark+)

(setq-default
 delete-by-moving-to-trash t)

(setq delete-auto-save-files t
      display-line-numbers-type nil
      inhibit-startup-message t
      mouse-wheel-follow-mouse t
      show-paren-mode 1
      line-number-mode t
      column-number-mode t

      ;; menu-bar
      menu-bar-mode -1
      scroll-bar-mode -1
      tool-bar-mode -1
      visible-bell t

      undo-limit 80000000
      ;; desktop file
      ;; (desktop-save-mode 1)
      ;; (setq desktop-save t)
      ;; (setq desktop-restore-eager 10)
      ;; (setq desktop-path '("~/.emacs.d/desktop/"))
      auto-save-default t
      global-auto-revert-mode 1

      c-auto-newline t
      tab-width 2
      indent-tabs-mode nil

      ;; disable auto line-breaking
      auto-fill-mode -1

      backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(display-time-mode 1)
(global-subword-mode 1)

(setq org-directory "~/GatsbyDrive/org/")

(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
;; (defvaralias 'c-basic-offsett 'tab-width)
;; (defvaralias 'cperl-indent-level 'tab-width)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;(put 'upcase-region 'disabled nil)
;(add-to-list 'global-mode-string '(" %i"))

;; disable pretty-code
(setq global-prettify-symbols-mode -1)
(setq +pretty-code-enabled-modes nil)
(remove-hook 'after-change-major-mode-hook #'+pretty-code-init-pretty-symbols-h)

(pyenv-mode -1)

;; Custom variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;(global-spell-fu-mode -1)
;(spell-fu-mode-disable)

;;;
;;; ivy/counsel
;;;
(after! ivy
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  (setq ivy-truncate-lines nil)
  (setq ivy-display-style 'fancy)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-wrap t)
  ;(when (setq enable-recursive-minibuffers t)
  ;  (minibuffer-depth-indcate-mode 1))
  (setq ivy-initial-inputs-alist
        '((counsel-minor . "")
          (counsel-package . "")
          (counsel-org-capture . "")
          (counsel-M-x . "")
          (counsel-describe-function . "")
          (counsel-describe-variable . "")))
  (setq ivy-read-action-function #'ivy-hydra-read-action)

  (defadvice! prompt-for-buffer (&rest _)
    :after '(evil-window-split evil-window-vsplit)
    (+ivy/switch-buffer))

  (setq +ivy-buffer-preview t)

  (setq +ivy-project-search-engines '(rg))
  (ivy-add-actions
     'counsel-M-x
     `(("h" +ivy/helpful-function "Helpful")))
  (ivy-mode 1)
  (ivy-rich-mode 1))

(after! counsel
  (counsel-mode 1))
  ;; :bind
  ;; ("M-x" . 'counsel-M-x)
  ;; ("M-y" . 'counsel-yank-pop)
  ;; ("M-s" . 'counsel-ibuffer))

;;;
;;; projectile
;;;
(after! projectile
  (projectile-mode 1)
  (projectile-load-known-projects))

;; dired
;; (after! dired-k
;;     (setq dired-k-human-readable t))
;;
;; hl-todo
;;
(use-package! hl-todo
  ;; it, e.g. python-mode)
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"))

;;;
;;; company
;;;
;; company
(after! company
  ;; :bind (
  ;;        :map company-active-map
  ;;        ("M-n" . nil)
  ;;        ("M-p" . nil)
  ;;        ("C-n" . company-select-next)
  ;;        ("C-p" . company-select-previous)
  ;;        ("C-h" . nil)
  ;;        ("C-s" . company-filter-candidates)
  ;;        ("C-i" . company-complete-selection)
  ;;        ("<tab>" . company-complete-common-or-cycle)
  ;;        ("M-d" . company-show-doc-buffer)
  ;;        :map company-search-map
  ;;        ("C-n" . company-select-next)
  ;;        ("C-p" . company-select-previous))
  :config
  (global-company-mode +1)
  (setq company-quickhelp-mode +1)
  ;; (custom-set-variables '(company-idle-delay nil))
  (setq company-transformers '(company-sort-by-backend-importance))
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t))
  ;; (setq company-dabbrev-downcase nil)
  ;; Add yasnippet support for all company backends.
  ;; (defvar company-mode/enable-yas t "Enable yasnippet for all backends."))
;;  (company-quickhelp-mode))

;;
;; yasnippet
;;
(after! yasnippet
  (yas-global-mode))

;;;
;;; tramp
;;;
(use-package! counsel-tramp
  :commands (counsel-tramp))
(setq tramp-auto-save-directory "~/tmp/tramp/")
(setq tramp-chunksize 2000)

;;;
;;; which-key
;;;
(use-package! which-key
  :config
  (setq which-key-idle-secondary-delay 0.05))
(which-key-mode)

;; kubernetes
(after! k8s-mode
  :hook (k8s-mode . yas-minor-mode)
  :config
  (setq k8s-indent-offset nil
        k8s-site-docs-version "v1.17"))

(after! kubernetes
  :commands (kubernetes-overview))

;;;
;;; selectrum
;;;
(use-package! selectrum
  :config
  (selectrum-mode +1)
  )

;;;
;;; doom-modeline
;;;
;; (use-package! doom-modeline
;;   :hook (after-init . doom-modeline-mode)
;;   :custom
;;   (doom-modeline-height 25)
;;   (doom-modeline-bar-width 1)
;;   (doom-modeline-icon t)
;;   (doom-modeline-major-mode-icon t)
;;   (doom-modeline-major-mode-color-icon t)
;;   (doom-modeline-buffer-file-name-style 'truncate-upto-project)
;;   (doom-modeline-buffer-state-icon t)
;;   (doom-modeline-buffer-modification-icon t)
;;   (doom-modeline-minor-modes nil)
;;   (doom-modeline-enable-word-count nil)
;;   (doom-modeline-buffer-encoding t)
;;   (doom-modeline-indent-info nil)
;;   (doom-modeline-checker-simple-format t)
;;   (doom-modeline-vcs-max-length 12)
;;   (doom-modeline-env-version t)
;;   (doom-modeline-irc-stylize 'identity)
;;   (doom-modeline-github-timer nil)
;;   (doom-modeline-gnus-timer nil) )

;;;
;;; prog-mode
;;;
(format-all-mode -1)

(after! lsp-mode
  :config
  (setq lsp-enable-snippet t
        lsp-auto-guess-root t
        lsp-enable-semantic-highlighting t
        ;; (lsp-inhibit-message t)
        lsp-message-project-root-warning t
        create-lockfiles nil)
  ;;:hook
  ;;(prog-major-mode . lsp-prog-major-mode-enable)
  :commands lsp)

(use-package! company-lsp
  :after lsp-mode
  :commands company-lsp
  :custom
  (push 'company-lsp company-backends))

(use-package! lsp-ui
  :after lsp-mode
  :custom
  (scroll-margin 0)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-peek-enable t)
  :hook (lsp-mode . lsp-ui-mode))

(after! flycheck
  :custom
  (setq flycheck-check-syntax-automatically '(mode-enabled save)
        flycheck-display-errors-delay 0.2
        flycheck-display-errors-function nil
        flycheck-idle-change-delay 1.0)
  :config
  (global-flycheck-mode t) )

(use-package! flycheck-popup-tip
  :after (flycheck)
  :hook (flycheck-mode . flycheck-popup-tip-mode) )

(use-package! bazel-build)
(use-package! bazel
  :mode (("\\.bzl\\'" . bazel-build-mode)
         ("BUILD\\'" . bazel-build-mode)
         ("WORKSPACE\\'" . bazel-build-mode))
  :hook (before-save-hook . bazel-mode-buildifier)
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

(after! go-mode
  :custom
  (setq tab-width 2)
  (setq completion-ignore-case t)
  ;; :bind
  ;; ("M-." . godef-jump)
  ;; ("C-M-i" . company-complete)
  :hook
  (before-save-hook . gofmt-before-save))
(after! company-go
  :custom
  (add-to-list 'company-backends 'company-go))
(after! go-eldoc
  :hook
  (go-mode-hook . go-eldoc-setup))

;;
;; Whitespace
;;
(use-package! whitespace
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

(prettify-symbols-mode -1)

;; the use-package!, after!, add-hook! and setq-hook! macros are your bread and butter.
;;
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(load! "+bindings")
(load! "+org")

(provide 'config)
;;; config.el ends here
