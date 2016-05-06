(require 'git-blame)
(require 'gitignore-mode)
(require 'gitconfig-mode)
(require 'gitconfig-mode)
(require 'git-timemachine)
(require 'git-commit)
(require 'magit)

;(require-package 'fullframe)
(after-load 'magit
						(fullframe magit-status magit-mode-quit-window))

(add-hook 'git-commit-mode-hook 'goto-address-mode)
