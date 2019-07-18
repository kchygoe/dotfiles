;;; init-misc.el --- misc tasks
;;; Commentary:
;;; Code:

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

;; tree-undo
(use-package undo-tree)
;; (global-undo-tree-mode) ; set tree-undo as default
;; C-M-z
;; (bind-key "C-M-z" 'undo-tree-redo)

;; expand-region
(use-package expand-region
  :config
  (bind-key "C-=" 'er/expand-region))

;; PowerLine
(use-package powerline
  :config
  (powerline-default-theme)
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

(provide 'init-misc)
;;; init-misc.el ends here
