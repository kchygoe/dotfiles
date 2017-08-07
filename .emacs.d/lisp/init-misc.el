;;; init-misc.el --- misc tasks
;;; Code:

;; neotree
(use-package neotree)
(bind-key "<f8>" 'neotree-toggle)

; ido-mode
(use-package ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

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
(use-package powerline)
(powerline-default-theme)
(setq powerline-arrow-shape 'arrow)

;; golden ratio
(use-package golden-ratio)
(golden-ratio-mode 1)

;; rainbow-delimiters
(use-package rainbow-delimiters)
(rainbow-delimiters-mode 1)
;;(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

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
(use-package multiple-cursors)
(bind-key "C-S-c C-S-c" 'mc/edit-lines)
(bind-key "C->" 'mc/mark-next-like-this)
(bind-key "C-<" 'mc/mark-previous-like-this)
(bind-key "C-c C-<" 'mc/mark-all-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(bind-key "M-<mouse-1>" 'mc/add-cursor-on-click)

;; migemo
;; (use-package helm-migemo)
;; (with-eval-after-load "helm-migemo"
;;   (defun helm-compile-source--candidates-in-buffer (source)
;;     (helm-aif (assoc 'candidates-in-buffer source)
;;         (append source
;;                 `((candidates
;;                    . ,(or (cdr it)
;;                           (lambda ()
;;                             ;; Do not use `source' because other plugins
;;                             ;; (such as helm-migemo) may change it
;;                             (helm-candidates-in-buffer (helm-get-current-source)))))
;;                   (volatile) (match identity)))
;;       source))
;;   (defalias 'helm-mp-3-get-patterns 'helm-mm-3-get-patterns)
;;   (defalias 'helm-mp-3-search-base 'helm-mm-3-search-base))
;; (setq helm-use-migemo t)
;; (setq migemo-command "cmigemo")
;; (setq migemo-options '("-q" "--emacs"))
;; (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
;; (load-library "migemo")
;; (migemo-init)

;; mutiple-cursors
(use-package multiple-cursors)
(bind-key "C-S-c C-S-c" 'mc/edit-lines)
(bind-key "C->" 'mc/mark-next-like-this)
(bind-key "C-<" 'mc/mark-previous-like-this)
(bind-key "C-c C-<" 'mc/mark-all-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(bind-key "M-<mouse-1>" 'mc/add-cursor-on-click)

;; col-highlight
(use-package col-highlight)
;; (column-highlight-mode 1)

;; smartparens
(use-package smartparens)
(smartparens-global-mode)

;; mmm-mode
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)

;; smart-newline
(use-package smart-newline
  :config
  (progn
    (bind-key "C-m" 'smart-newline)))


;;; init-misc.el ends here
