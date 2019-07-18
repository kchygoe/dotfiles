;;; init-git.el -- set git
;;; Code:

(use-package git-blamed)
(use-package git-gutter)
(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package git-timemachine)
(use-package git-commit)
(use-package magit)

(use-package fullframe)
(add-hook 'git-commit-mode-hook 'goto-address-mode)

;; git-gutter+
(global-git-gutter-mode +1)

(provide 'init-git)
;;; init-git.el ends here
