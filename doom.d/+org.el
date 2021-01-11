 ;;; ~/src/github.com/kchygoe/dotfiles/doom.d/+org.el -*- lexical-binding: t; -*-

;;;
 ;;; Org
;;;
(use-package! org
  :mode ("\\.org$" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))

:config
(setq org-default-notes-file (concat org-directory "/note.org"))
(setq org-agenda-files '("~/GatsbyDrive/org/" "~/.org-jira/" "~/GatsbyDrive/org/gcal/" ))
(setq org-return-follows-link t)
(setq org-log-done 'time)
(setq org-startup-truncated nil)
(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
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
:hook
(org-agenda-mode-hook . (lambda ()
                          (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
                          (auto-save-mode)))
(org-mode . org-bullets-mode))

;; agenda
(setq org-agenda-custom-commands
      '(("x" "Unscheduled Tasks" tags-todo
         "-SCHEDULED>=\"<today>\"-DEADLINE>=\"<today>\"" nil)
        ("d" "Daily Tasks" agenda ""
         ((org-agenda-span 1)))))
(setq org-agenda-skip-scheduled-if-done t)

;; org-gcal
(after! org (use-package! org-gcal
  ;; :hook
  ;; (org-agenda-mode-hook . org-gcal-sync)
  :config
  (setq org-gcal-client-id (getenv "GCAL_CLIENT_ID")
        org-gcal-client-secret (getenv "GCAL_CLIENT_SECRET")
        org-gcal-file-alist '(("gatsby.gatsby.gatsby@gmail.com" . "~/GatsbyDrive/org/gcal/gcal.org")
                              ("yoshigoe@leapmind.io" . "~/GatsbyDrive/org/gcal/gcal-work.org") ))))

;; github
(setq org-github-issues-org-file (concat org-directory "/github.org"))

;; org-roam
(use-package! org-roam
  :after (org)
  ;;:hook (org-mode . org-roam-mode)
  :config
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
  (defconst org-jira-progress-issue-flow
  '(("To Do" . "In Progress")
    ("In Progress" . "Review")
    ("Review" . "DONE")))))

;; asana
;; USE ASANA_TOKEN in env
;(use-package! asana)

;; For babel-load
;;(require 'org)
;;(org-babel-load-file
 ;;(expand-file-name "initial.org" user-emacs-directory))

;; arhive command
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))


(provide '+org)
