;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Code:

(setq user-full-name "Koichi Yoshigoe"
      user-mail-address "yoshigoe@leapmind.io")

(setq doom-font (font-spec :family "monospace" :size 14))
(setq doom-theme 'doom-dark+)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/GatsbyDrive/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(setq inhibit-startup-message t)
(setq delete-auto-save-files t)
(setq frame-title-format "%f")
(setq c-auto-newline t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(fset 'yes-or-no-p 'y-or-n-p)

;;(setq mouse-wheel-follow-mouse t)
(setq tab-width 2)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
;; (defvaralias 'c-basic-offsett 'tab-width)
;; (defvaralias 'cperl-indent-level 'tab-width)

(put 'upcase-region 'disabled nil)
(show-paren-mode 1)
(line-number-mode t)
(column-number-mode t)
(add-to-list 'global-mode-string '(" %i"))

;; menu-bar
(menu-bar-mode -1)

;; desktop file
(desktop-save-mode 1)
(setq desktop-save t)
(setq desktop-restore-eager 10)
(setq desktop-path '("~/.emacs.d/desktop/"))
(global-auto-revert-mode 1)

;; Custom variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;;
;;; bind
;;;
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
(bind-key "C-c c" 'org-capture)

;;;
;;; Org
;;;
(use-package! org
  :mode ("\\.org$" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (setq org-default-notes-file (concat org-directory "/note.org"))
  (setq org-agenda-files '("~/GatsbyDrive/org/"))
  (setq org-log-done 'time)
  (setq org-startup-truncated nil)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c@)")))
  (setq org-capture-templates
        '(("a" "Appointment" entry (file  "~/GatsbyDrive/org/gcal.org" )
           "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
          ("l" "Link" entry (file+headline "~/GatsbyDrive/org/links.org" "Links")
           "* %? %^L %^g \n%T" :prepend t)
          ("i" "Idea" entry (file+headline "~/GatsbyDrive/org/idea.org" "Idea Topics:")
           "* %?\n%T" :prepend t)
          ("t" "To Do Item" entry (file+headline "~/GatsbyDrive/org/note.org" "INBOX")
           "* TODO %?\n%u" :prepend t)
          ("n" "Note" entry (file+headline "~/GatsbyDrive/org/note.org" "NOTE SPACE")
           "* %?\n%u" :prepend t)
          ("j" "Journal" entry (file+datetree "~/GatsbyDrive/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
              (auto-save-mode))) )

(add-hook! org-mode org-bullets-mode)

;; org-gcal
(use-package! org-gcal
  ;; :hook
  ;; (org-agenda-mode-hook . org-gcal-sync)
  :config
  (setq org-gcal-client-id (getenv "GCAL_CLIENT_ID")
        org-gcal-client-secret (getenv "GCAL_CLIENT_SECRET")
        org-gcal-file-alist '(("gatsby.gatsby.gatsby@gmail.com" . "~/GatsbyDrive/org/gcal.org")
                              ("yoshigoe@leapmind.io" . "~/GatsbyDrive/org/gcal-work.org") )))

;; org-roam
(use-package! org-roam
  :after (org)
  :hook (org-mode . org-roam-mode)
  :custom
  (org-roam-directory org-directory)
  :bind
  ("C-c n l" . org-roam)
  ("C-c n t" . org-roam-today)
  ("C-c n f" . org-roam-find-file)
  ("C-c n i" . org-roam-insert)
  ("C-c n g" . org-roam-show-graph))

;; shortcut for checking note.org
(defun show-org-buffer (file)
  "Show an org-file FILE on the current buffer."
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat "~/GatsbyDrive/org/" file))))
(global-set-key (kbd "C-^") '(lambda () (interactive)
                               (show-org-buffer "note.org")))

;;(use-package org-jira
;;  :custom
;;  (setq jiralib-url "https://leapmind.atlassian.net"))

;; asana
;; USE ASANA_TOKEN in env
(use-package! asana)


;;;
;;; prog-mode
;;;

(use-package! bazel-build)
(use-package! bazel
  :mode (("\\.bzl\\'" . bazel-mode)
         ("BUILD\\'" . bazel-mode)
         ("WORKSPACE\\'" . bazel-mode))
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

(provide 'config)
;;; config.el ends here
