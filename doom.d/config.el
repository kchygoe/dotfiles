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
      doom-theme 'doom-dark+
      +doom-dashboard-name "emacs")

(set-frame-parameter (selected-frame) 'alpha '(90 . 75))

(setq-default delete-by-moving-to-trash t)

(setq delete-auto-save-files t
      display-line-numbers-type nil
      inhibit-startup-message t
      mouse-wheel-follow-mouse t
      show-paren-mode 1
      line-number-mode t
      column-number-mode t

      ;; menu-bar
      menu-bar-mode nil
      scroll-bar-mode nil
      tool-bar-mode nil
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
;; (put 'upcase-region 'disabled nil)
;; (add-to-list 'global-mode-string '(" %i"))

(pyenv-mode -1)

(after! ivy
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist
        '((t . ivy--regex-ignore-order))
        ivy-truncate-lines nil
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil
        ivy-wrap t)
  ;; (when (setq enable-recursive-minibuffers t)
  ;; (minibuffer-depth-indcate-mode 1))
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

  (setq +ivy-buffer-preview t
        +ivy-project-search-engines '(rg))
  (ivy-add-actions
   'counsel-M-x
   `(("h" +ivy/helpful-function "Helpful")))
  (ivy-mode 1)
  (ivy-rich-mode 1))

(after! counsel
  (counsel-mode 1))
;; :bind
;; ("M-y" . 'counsel-yank-pop)
;; ("M-s" . 'counsel-ibuffer))

(after! projectile
  (projectile-mode 1)
  (projectile-load-known-projects))

(use-package! hl-todo
  ;; it, e.g. python-mode)
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode 1))

(after! yasnippet
  (yas-global-mode))

(use-package! counsel-tramp
  :commands (counsel-tramp))
(setq tramp-auto-save-directory "~/tmp/tramp/")
(setq tramp-chunksize 2000)

(use-package! company
  :bind
  (:map company-active-map
   (("C-j" . company-select-next)
    ("C-n" . company-select-next)
    ("C-k" . company-select-previous)
    ("C-p" . company-select-previous)
    ("C-d" . company-show-doc-buffer)
    ("M-n" . nil)
    ("M-p" . nil)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("C-h" . nil)
    ("C-s" . company-filter-candidates)
    ("C-i" . company-complete-selection)
    ("<tab>" . company-complete-common-or-cycle)
    ("M-d" . company-show-doc-buffer))
   :map company-search-map
   (("C-n" . company-select-next)
    ("C-p" . company-select-previous)))
  :custom
  (company-idle-delay nil)
  :config
  (global-company-mode +1)
  (setq company-quickhelp-mode +1
        company-idle-delay 0
        company-transformers '(company-sort-by-backend-importance)
        company-minimum-prefix-length 3
        company-selection-wrap-around t
        completion-ignore-case t)
  (set-company-backend! 'org-mode '(company-yasnippet company-capf company-files company-elisp))
  (add-to-list 'company-backends '(company-capf company-files company-yasnippet)))

;; (setq company-dabbrev-downcase nil)

(use-package! company-box
  :hook (company-mode . company-box-mode))

(use-package! selectrum
  :config
  (selectrum-mode +1))

(use-package! which-key
  :config
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(use-package! visual-fill-column
  :config
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t))

(prettify-symbols-mode -1)
(global-prettify-symbols-mode -1)
(setq +pretty-code-enabled-modes nil)
(remove-hook 'after-change-major-mode-hook #'+pretty-code-init-pretty-symbols-h)

(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))

(map!
 ;; general
 "<C-return>" #'other-window
 "C-c i" #'indent-region
 "C-c C-i" #'dabbrev-expand
 "C-c ;" #'comment-region
 "C-c :" #'uncomment-region
 "C-s" #'swiper
 "C-c s" #'query-replace
 "C-u" #'scroll-down
 "C-h" #'delete-backward-char
 "M-?" #'help-for-help

 ;; ivy
 "C-x b" #'ivy-switch-buffer

 ;; avy
 "C-:" #'avy-goto-char
 "C-'" #'avy-goto-char-2

 ;; org-mode
 "C-c l"  #'org-store-link
 "C-c a"  #'org-agenda

 ;; visual-regexp
 "C-c r" #'vr/replace
 "C-c q" #'vr/query-replace
 "C-c m" #'vr/mc-mark

 ;; org-roam
 "C-c n l" #'org-roam
 "C-c n t" #'org-roam-today
 "C-c n f" #'org-roam-find-file
 "C-c n i" #'org-roam-insert
 "C-c n g" #'org-roam-show-graph
 )

;;;
;;; old style bind
;;;
;; (bind-key "<C-return>" 'other-window)
;; (bind-key "C-c i" 'indent-region)
;; (bind-key "C-c C-i" 'dabbrev-expand)
;; (bind-key "C-c ;" 'comment-region)
;; (bind-key "C-c :" 'uncomment-region)
;; (bind-key "C-c s" 'query-replace)
;; (bind-key "C-u" 'scroll-down)
;; (bind-key "C-h" 'delete-backward-char)
;; (bind-key "M-?" 'help-for-help)
;; (bind-key "M-n" 'goto-line)
;; (bind-key "C-c c" 'org-capture)

(defun my/org-setup()
  (org-indent-mode)
  (org-bullets-mode)
  (visual-line-mode 1))

(use-package! org
  :mode
  ("\\.org$" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :config
  (setq org-ellipsis " ▼ "
        org-default-notes-file (concat org-directory "/note.org")
        org-agenda-files '("~/GatsbyDrive/org/" "~/GatsbyDrive/org/jira/" "~/GatsbyDrive/org/gcal/" )
        org-return-follows-link t
        org-log-done 'time
        org-startup-truncated nil
        org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (setq org-todo-keywords
        ;; Sequence for TASKS
        '((sequence "TODO(t)" "SOMEDAY(s)" "InProgress(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c@)")))
  (setq org-capture-templates
        '(("a" "Appointment" entry (file  "~/GatsbyDrive/org/gcal.org" )
           "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
          ("l" "Link" entry (file+headline "~/GatsbyDrive/org/links.org" "Links")
           "* %? %^L %^g \n%T" :prepend t)
          ("i" "Idea" entry (file+headline "~/GatsbyDrive/org/idea.org" "Idea Topics:")
           "* %?\n%T" :prepend t)
          ("t" "To Do Item" entry (file+headline "~/GatsbyDrive/org/note.org" "INBOX")
           "* TODO %?\n%u\n" :prepend t)
          ("n" "Note" entry (file+headline "~/GatsbyDrive/org/note.org" "NOTE SPACE")
           "* %?\n%u\n" :prepend t)
          ("j" "Journal" entry (file+datetree "~/GatsbyDrive/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  :hook
  (org-agenda-mode-hook . (lambda ()
                            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
                            (auto-save-mode)))
  (org-mode . my/org-setup))

;; agenda
(setq org-agenda-custom-commands
      '(("x" "Unscheduled Tasks" tags-todo
         "-SCHEDULED>=\"<today>\"-DEADLINE>=\"<today>\"" nil)
        ("d" "Daily Tasks" agenda ""
         ((org-agenda-span 1)))))
(setq org-agenda-skip-scheduled-if-done t)
(setq org-babel-python-command "python3")

(after! org
  (use-package! org-tempo)
  (use-package! org-gcal
    :hook
    (org-agenda-mode-hook . (lambda () (org-gcal-sync) ))
    (org-capture-after-finalize-hook . (lambda() (org-gcal-sync)))
    :config
    (setq org-gcal-client-id (getenv "GCAL_CLIENT_ID")
          org-gcal-client-secret (getenv "GCAL_CLIENT_SECRET")
          org-gcal-file-alist '(("gatsby.gatsby.gatsby@gmail.com" . "~/GatsbyDrive/org/gcal/gcal.org")
                                ("yoshigoe@leapmind.io" . "~/GatsbyDrive/org/gcal/gcal-work.org"))
          org-gcal-up-days 7)))

;; github
(setq org-github-issues-org-file (concat org-directory "/github.org"))

;; org-roam
(use-package! org-roam
  :after org
  :hook
  (org-mode . org-roam-mode)
  :init
  (setq org-roam-directory org-directory)
  :bind
  ("C-c n l" . org-roam)
  ("C-c n t" . org-roam-today)
  ("C-c n f" . org-roam-find-file)
  ("C-c n i" . org-roam-insert)
  ("C-c n g" . org-roam-show-graph))

;; (after! org-roam (use-package! company-org-roam
;;  :config
;;  (push 'company-org-roam company-backends)))

;; shortcut for note.org
(defun my/show-org-buffer (file)
  "Show an org-file FILE on the current buffer."
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat org-directory file))))
(global-set-key (kbd "C-^") '(lambda () (interactive)
                               (my/show-org-buffer "note.org")))

;; Github
(use-package! org-sync
  :config
  (defvar org-sync-backend-alist
    '(("github.com/\\(?:repos/\\)?[^/]+/[^/]+"  . org-sync-github-backend))))

;; JIRA
;; Refs. https://github.com/ahungry/org-jira
(after! org (use-package! org-jira
              :config
              (setq jiralib-url "https://leapmind.atlassian.net")
              (setq org-jira-working-dir (concat org-directory "jira"))
              (defconst org-jira-progress-issue-flow
                '(("To Do" . "In Progress")
                  ("In Progress" . "Review")
                  ("Review" . "DONE")))))

;; asana
;; USE ASANA_TOKEN in env
;; (use-package! asana)

;; arhive command
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(use-package! org-super-agenda
  :commands (org-super-agenda-mode))
(after! org-agenda
  (org-super-agenda-mode))

(use-package! deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-directory))

(format-all-mode -1)

(after! lsp-mode
  :config
  (setq lsp-enable-snippet t
        lsp-auto-guess-root t
        lsp-enable-semantic-highlighting t
        ;; (lsp-inhibit-message t)
        lsp-message-project-root-warning t
        create-lockfiles nil)
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-prefer-capf t
        lsp-headerline-breadcrumb-mode t)
  (setq lsp-terraform-server "/usr/local/bin/terraform-ls")
  (setq lsp-terraform-enable-logging t)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("/usr/local/bin/terraform-ls" "serve"))
                    :major-modes '(terraform-mode)
                    :server-id 'terraform-ls))
  :hook
  (terraform-mode-hook . #'lsp-deferred)
  ;; (prog-major-mode . lsp-prog-major-mode-enable)
  )


(use-package! lsp-ui
  :after lsp-mode
  :custom
  (scroll-margin 0)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-peek-enable t)
  :hook
  (lsp-mode . lsp-ui-mode))

(after! flycheck
  :custom
  (setq flycheck-check-syntax-automatically '(mode-enabled save)
        flycheck-display-errors-delay 0.2
        flycheck-display-errors-function nil
        flycheck-idle-change-delay 1.0)
  :config
  (global-flycheck-mode t) )

(use-package! flycheck-popup-tip
  :after flycheck
  :hook
  (flycheck-mode . flycheck-popup-tip-mode))

(use-package! bazel-build)
(use-package! bazelrc-mode
  :mode ((".bazelrc" . bazelrc-mode)))
(use-package! bazel-mode)
(use-package! bazel
  :mode (("\\.bzl\\'" . bazel-build-mode)
         ("BUILD\\'" . bazel-build-mode)
         ("WORKSPACE\\'" . bazel-build-mode))
  :hook (before-save-hook . bazel-mode-buildifier)
  :config
  (setq bazel-mode-buildifier-before-save t)
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

;;(after! go-mode
;;:config
;; (setq tab-width 2)
;;(setq completion-ignore-case t)
;;:bind
;;("M-." . godef-jump)
;;("C-M-i" . company-complete)
;;:hook
;;(before-save-hook . gofmt-before-save))
;; (after! company-go
;;   :config
;;   (add-to-list 'company-backends 'company-go))
;; (after! go-eldoc
;;   :hook
;;   (go-mode-hook . go-eldoc-setup))

;; (after! yaml-mode
;;   :config
;;   (setq yaml-indent-offset 2))

(after! k8s-mode
  :hook
  (k8s-mode . yas-minor-mode)
  :config
  (setq k8s-indent-offset nil
        k8s-site-docs-version "v1.18"))

(after! kubernetes
  :commands (kubernetes-overview))

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
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]))
        whitespace-action '(auto-cleanup))
  ;;(setq whitespace-space-regexp "\\(\u3000+\\)")
  (global-whitespace-mode 1)

  (defvar my/bg-color "#272727")
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
                      :background my/bg-color))

;; Custom variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

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
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how;
                                        ; they are implemented.

(provide 'config)
;;; config.el ends here
