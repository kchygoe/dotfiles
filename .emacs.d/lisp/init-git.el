;;; init-git.el -- set git
;;; Code:

(use-package git-blame)
(use-package git-gutter)
(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package gitconfig-mode)
(use-package git-timemachine)
(use-package git-commit)
(use-package magit)

;(require-package 'fullframe)
(after-load 'magit
            (fullframe magit-status magit-mode-quit-window))

(add-hook 'git-commit-mode-hook 'goto-address-mode)

;; git-gutter+
(global-git-gutter-mode +1)


;;; init-git.el ends here
