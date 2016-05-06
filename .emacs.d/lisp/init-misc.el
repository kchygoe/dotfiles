;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


;; tree-undo
(el-get-bundle undo-tree)
(global-undo-tree-mode t) ; set tree-undo as default
;; C-M-z
(define-key global-map (kbd "C-M-z") 'undo-tree-redo)


;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;; PowerLine
(require 'powerline)
(powerline-default-theme)
(setq powerline-arrow-shape 'arrow)


;; rainbow-delimiters
(require 'rainbow-delimiters)
(rainbow-delimiters-mode 1)
;;(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; smooth-scroll
;; (require 'smooth-scroll)
;; (smooth-scroll-mode t)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; (setq mouse-wheel-progressive-speed nil) don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) scroll window under mouse
;; (setq scroll-step 1)
;; (setq scroll-conservatively 10000)
;; (defcustom smooth-scroll/hscroll-step-size 8)
;; (defcustom smooth-scroll/vscroll-step-size 8)


;; mutiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
