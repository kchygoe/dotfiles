;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Code:
;;
;; Basic configs
;;

(setq user-full-name "Koichi Yoshigoe"
      user-mail-address "koichi.yoshigoe@gmail.com")

;; doom basics
(setq doom-font (font-spec :family "Hack Nerd Font" :size 18)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font Mono" :size 18)
      doom-serif-font (font-spec :family "UbuntuMono Nerd Font" :weight 'Regular)
      doom-unicode-font (font-spec :family "Hack Nerd Font Mono" :size 18)
      doom-big-font (font-spec :family "Hack Nerd Font Mono" :size 24)
      doom-theme 'doom-dark+
      +doom-dashboard-name "emacs")
(doom-themes-org-config)

(set-frame-parameter (selected-frame) 'alpha '(93 . 75))

(setq-default delete-by-moving-to-trash t)

;; lsp-mode tuning
(setq gc-cons-threshold 100000000
      read-process-output-max (* 2048 2048))

(setq delete-auto-save-files t
      display-line-numbers-type nil
      inhibit-startup-message t
      mouse-wheel-follow-mouse t
      show-paren-mode 1
      line-number-mode t
      column-number-mode t

      menu-bar-mode nil
      scroll-bar-mode nil
      tool-bar-mode nil
      visible-bell nil

      undo-limit 8000000
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
(setq display-time-24hr-format t
      display-time-format "%H:%M")

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

(set-language-environment "Japanese")

(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))

(use-package! projectile
  :config
  (projectile-mode +1)
  (projectile-load-known-projects))

(use-package! hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode 1))

(use-package! counsel-tramp
  :commands (counsel-tramp))
(setq tramp-auto-save-directory "~/tmp/tramp/")
(setq tramp-chunksize 2000)

(after! company
  ;; :bind
  ;; (:map company-active-map
  ;;  (("C-j" . company-select-next)
  ;;   ("C-n" . company-select-next)
  ;;   ("C-k" . company-select-previous)
  ;;   ("C-p" . company-select-previous)
  ;;   ("C-d" . company-show-doc-buffer)
  ;;   ("M-n" . nil)
  ;;   ("M-p" . nil)
  ;;   ("C-n" . company-select-next)
  ;;   ("C-p" . company-select-previous)
  ;;   ("C-h" . nil)
  ;;   ("C-s" . company-filter-candidates)
  ;;   ("C-i" . company-complete-selection)
  ;;   ("<tab>" . company-complete-common-or-cycle)
  ;;   ("M-d" . company-show-doc-buffer))
  ;;  :map company-search-map
  ;;  (("C-n" . company-select-next)
  ;;   ("C-p" . company-select-previous)))
  :config
  (setq company-quickhelp-mode +1
        company-idle-delay 0.2
        company-transformers '(company-sort-by-backend-importance)
        company-minimum-prefix-length 3
        company-selection-wrap-around t
        completion-ignore-case t))

;;(add-to-list 'company-backends '(company-capf company-files company-yasnippet)))

(use-package! selectrum
  :config
  (selectrum-mode +1))

(use-package! which-key
  :config
  (setq which-key-idle-secondary-delay 0.05
        which-key-idle-delay 0.6)
  (which-key-mode))

(when (equal system-type 'darwin)
  ;; (setq mac-option-modifier 'super)
  ;; (setq mac-command-modifier 'meta)
  (setq ns-auto-hide-menu-bar nil)
  (setq ns-use-proxy-icon nil)
  (setq initial-frame-alist
        (append
         '((ns-transparent-titlebar . nil)
           (ns-appearance . dark)
           (vertical-scroll-bars . nil)
           (internal-border-width . 0)))))

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
          ("i" "Idea" entry (file+headline "~/GatsbyDrive/org/note.org" "Idea Topics:")
           "* %?\n%T" :prepend t)
          ("t" "To Do Item" entry (file+headline "~/GatsbyDrive/org/note.org" "INBOX")
           "* TODO %?\n%u\n" :prepend t)
          ("n" "Note" entry (file+headline "~/GatsbyDrive/org/note.org" "NOTE SPACE")
           "* %?\n%u\n" :prepend t)
          ("m" "MTG" entry (file+headline "~/GatsbyDrive/org/note.org" "MTG Log")
           "* %?\n%u\n" :prepend t)
          ("j" "Journal" entry (file+datetree "~/GatsbyDrive/org/journal.org")
           "* %?\nEntered on %U\n %i\n %a")))
  (setq org-babel-python-command "python3")
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  :hook
  (org-agenda-mode-hook . (lambda ()
                            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
                            (auto-save-mode)))
  (org-mode . my/org-setup))

;; company
(after! org
  (set-company-backend! 'org-mode 'company-yasnippet 'company-capf 'company-files 'company-elisp))

;; agenda
(setq org-agenda-custom-commands
      '(("x" "Unscheduled Tasks" tags-todo
         "-SCHEDULED>=\"<today>\"-DEADLINE>=\"<today>\"" nil)
        ("d" "Daily Tasks" agenda ""
         ((org-agenda-span 1)))))
(setq org-agenda-skip-scheduled-if-done t)

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
  :after org
  :config
  (defvar org-sync-backend-alist
    '(("github.com/\\(?:repos/\\)?[^/]+/[^/]+"  . org-sync-github-backend))))

;; JIRA
;; Refs. https://github.com/ahungry/org-jira

(use-package! org-jira
  :after org
  :config
  (setq jiralib-url "https://leapmind.atlassian.net")
  (setq org-jira-working-dir (concat org-directory "jira"))
  (defconst org-jira-progress-issue-flow
    '(("To Do" . "In Progress")
      ("In Progress" . "Review")
      ("Review" . "DONE"))))

;; asana
;; USE ASANA_TOKEN in env
;; (use-package! asana)

;; arhive command
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(use-package! org-super-agenda
  :after org-agenda
  :commands (org-super-agenda-mode))

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
        lsp-inhibit-message t
        lsp-message-project-root-warning t
        lsp-enable-file-watchers nil
        create-lockfiles nil)
  (setq lsp-keymap-prefix "C-c l"
        ;;lsp-prefer-capf t
        lsp-headerline-breadcrumb-mode t)
  ;; :hook
  ;; (prog-major-mode . lsp-prog-major-mode-enable)
  )

(use-package! lsp-ui
  :after lsp-mode
  :config
  (setq scroll-margin 0
        lsp-ui-doc-enable t
        lsp-ui-doc-header t
        lsp-ui-peek-enable t)
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package! bazel
  :hook (before-save-hook . bazel-mode-buildifier)
  :config
  (setq bazel-buildifier-before-save t)
  (defun find-parent-directory-with-file(name)
    (projectile-locate-dominating-file (file-truename (buffer-file-name)) name)))

(after! go-mode
  (set-company-backend! 'go-mode 'company-go 'company-yasnippet)
  :config
  (setq tab-width 2))
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

(after! sh-script
  (set-company-backend! 'sh-mode '(company-shell :with company-yasnippet))
  (add-hook 'sh-mode #'lsp!))

(after! terraform-mode
  (setq lsp-terraform-enable-logging t)
  (add-hook 'terraform-mode-hook #'lsp!))

(after! k8s-mode
  :hook
  (k8s-mode . yas-minor-mode)
  :config
  (setq k8s-indent-offset nil
        k8s-site-docs-version "v1.18"))

(use-package! kubernetes
  :commands (kubernetes-overview))

(global-unset-key "\C-h")
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-?" 'help-for-help)
(global-set-key "\C-r" 'anzu-query-reqlace)
(global-set-key "\C-s" 'swiper-from-isearch)
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c:" 'uncomment-region)
  ;; (global-set-key "\C-cs" 'query-replace)
(global-set-key "\C-cu" 'scroll-down)

(map!
 ;;"<C-return>" #'other-window
 "C-c C-i" #'indent-region
 "C-c ;" #'comment-region
 "C-c :" #'uncomment-region
 "C-s" #'swiper-from-isearch
 "C-r" #'anzu-query-replace
 "C-c s" #'query-replace
 "C-c u" #'scroll-down
 ;;"M-/"  #'undo-tree-redo

 ;; ivy
 "C-x b" #'ivy-switch-buffer
 "C-c g" #'counsel-rg

 ;; avy
 "C-:" #'avy-goto-char
 "C-'" #'avy-goto-char-2

 ;; org-mode
 ;;"C-c l"  #'org-store-link
 "C-c a"  #'org-agenda

 ;; visual-regexp
 ;; "C-c r" #'vr/replace
 ;; "C-c q" #'vr/query-replace
 ;; "C-c m" #'vr/mc-mark

 ;; org-roam
 ;; "C-c n l" #'org-roam
 ;; "C-c n t" #'org-roam-today
 ;; "C-c n f" #'org-roam-find-file
 ;; "C-c n i" #'org-roam-insert
 ;; "C-c n g" #'org-roam-show-graph
 )

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
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how they are implemented.

(provide 'config)
;;; config.el ends here
