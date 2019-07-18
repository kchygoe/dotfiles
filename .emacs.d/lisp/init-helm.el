;;; init-helm -- helm config
;;; Commentary:
;;; Code:

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-c h" . helm-mini)
         ("C-c r" . helm-recentf)
         ("C-c C-h i" . helm-imenu)
         ("C-c C-h k" . helm-show-kill-ring)
         :map helm-map
         ("C-h" . delete-backward-char)
         ("TAB" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;; (define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)
  ;; (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  ;; (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; (define-key helm-read-file-map (kbd "C-z") 'helm-select-action)
  ;; (define-key helm-find-files-map (kbd "C-z") 'helm-select-action)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (helm-mode 1)
  (helm-descbinds-install)
  (helm-descbinds-mode +1)
  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  ;; (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  ;; (kill-new (buffer-substring (point) (field-end))))
  )
(use-package helm-config)

(custom-set-variables
 '(helm-input-idle-delay 0)
 '(helm-exit-idle-delay 0)
 '(helm-candidate-number-limit 500)
 '(helm-ag-insert-at-point 'symbol)
 '(helm-find-files-doc-header "")
 '(helm-command-prefix-key nil)
 '(helm-gtags-pulse-at-cursor nil) ;gtags
 )

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


(provide 'init-helm)
;;; init-helm.el ends here
