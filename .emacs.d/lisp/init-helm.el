;;; init-helm -- helm config
;;; Code:

(use-package helm)
(use-package helm-config)

(helm-mode +1)
(helm-descbinds-install)
(helm-descbinds-mode +1)

;; helm keybinds
(bind-key "M-x" 'helm-M-x)
(bind-key "C-c h" 'helm-mini)
(bind-key "C-c r" 'helm-recentf)
(bind-key  "C-c C-h i" 'helm-imenu)
(bind-key  "C-c C-h k" 'helm-show-kill-ring)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
;; Emulate `kill-line' in helm minibuffer
(setq helm-delete-minibuffer-contents-from-point t)
(defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  "Emulate `kill-line' in helm minibuffer"
  (kill-new (buffer-substring (point) (field-end))))
;; For find-file etc.
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

(custom-set-variables
 '(helm-input-idle-delay 0)
 '(helm-exit-idle-delay 0)
 '(helm-candidate-number-limit 500)
 '(helm-ag-insert-at-point 'symbol)
 '(helm-find-files-doc-header "")
 '(helm-command-prefix-key nil))

(with-eval-after-load 'helm
  (helm-descbinds-mode)
  (define-key helm-map (kbd "C-p")   #'helm-previous-line)
  (define-key helm-map (kbd "C-n")   #'helm-next-line)
  (define-key helm-map (kbd "C-M-n") #'helm-next-source)
  (define-key helm-map (kbd "C-M-p") #'helm-previous-source))

(with-eval-after-load 'helm-files
  (remove-hook 'post-self-insert-hook 'helm-find-files--reset-level-tree)
  (define-key helm-find-files-map (kbd "C-M-u") #'helm-find-files-down-one-level)
  (define-key helm-find-files-map (kbd "C-c C-o") #'helm-ff-run-switch-other-window))

;; gtags
(custom-set-variables
 '(helm-gtags-pulse-at-cursor nil))

(with-eval-after-load 'helm-gtags
  (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
  (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-t") 'helm-gtags-pop-stack))

;;; Enable helm-gtags-mode
(dolist (hook '(c-mode-common-hook
                java-mode-hook
                asm-mode-hook))
  (add-hook hook 'helm-gtags-mode))

;; migemo
;; (require 'helm-migemo)
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
;;   [2015-09-06 Sun]helm-match-plugin -> helm-multi-match変更の煽りを受けて
;;   (defalias 'helm-mp-3-get-patterns 'helm-mm-3-get-patterns)
;;   (defalias 'helm-mp-3-search-base 'helm-mm-3-search-base))
;; (setq helm-use-migemo t)
;; (setq migemo-command "cmigemo")
;; (setq migemo-options '("-q" "--emacs"))
;; (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
;; (load-library "migemo")
;; (migemo-init)

;;; init-helm.el ends here
