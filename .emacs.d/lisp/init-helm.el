;; helm
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)
(helm-descbinds-mode)
(when (require 'helm-config nil t)
	(helm-mode 1))
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

;; migemo
(require 'helm-migemo)
(with-eval-after-load "helm-migemo"
	(defun helm-compile-source--candidates-in-buffer (source)
		(helm-aif (assoc 'candidates-in-buffer source)
				(append source
								`((candidates
									 . ,(or (cdr it)
													(lambda ()
														;; Do not use `source' because other plugins
														;; (such as helm-migemo) may change it
														(helm-candidates-in-buffer (helm-get-current-source)))))
									(volatile) (match identity)))
			source))
	;; [2015-09-06 Sun]helm-match-plugin -> helm-multi-match変更の煽りを受けて
	(defalias 'helm-mp-3-get-patterns 'helm-mm-3-get-patterns)
	(defalias 'helm-mp-3-search-base 'helm-mm-3-search-base))
(setq helm-use-migemo t)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(load-library "migemo")
(migemo-init)
