;; PHP

(use-package php-mode)
(use-package smarty-mode)
(setq php-mode-force-pear t)

;; (defun my-fetch-php-completions ()
;;   (if (and (boundp 'my-php-symbol-list)
;;            my-php-symbol-list)
;;       my-php-symbol-list
;;     (message "Fetching completion list...")
;;     (with-current-buffer
;;         (url-retrieve-synchronously "http://www.php.net/manual/en/indexes.functions.php")
;;       (goto-char (point-min))
;;       (message "Collecting function names...")
;;       (setq my-php-symbol-list nil)
;;       (while (re-search-forward "<a[^>]*class=\"index\"[^>]*>\\([^<]+\\)</a>" nil t)
;;         (push (match-string-no-properties 1) my-php-symbol-list))
;;       my-php-symbol-list)))

;; (add-hook 'php-mode-hook
;;           (lambda ()
;;             (require 'php-completion) ))
;; 'my-fetch-php-completions))

;; ;; php
;; (setq php-mode-force-pear t)
;; (add-hook 'php-mode-hook
;;           (lambda ()
;;             (use-package php-completion) ))
;; 'my-fetch-php-completions))
;; (defun my-fetch-php-completions ()
;;   (if (and (boundp 'my-php-symbol-list)
;;            my-php-symbol-list)
;;       my-php-symbol-list
;;     (message "Fetching completion list...")
;;     (with-current-buffer
;;         (url-retrieve-synchronously "http://www.php.net/manual/en/indexes.functions.php")
;;       (goto-char (point-min))
;;       (message "Collecting function names...")
;;       (setq my-php-symbol-list nil)
;;       (while (re-search-forward "<a[^>]*class=\"index\"[^>]*>\\([^<]+\\)</a>" nil t)
;;         (push (match-string-no-properties 1) my-php-symbol-list))
;;       my-php-symbol-list)))
