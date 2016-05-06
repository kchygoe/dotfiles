;; Packages
;; others
(add-to-list 'load-path "~/.emacs.d/elpa/")

;; packages melpa
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; el-get
;; (when load-file-name
;; 	(setq user-emacs-directory (file-name-directory load-file-name)))

;; (add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
;; (unless (require 'el-get nil 'noerror)
;; 	(with-current-buffer
;; 			(url-retrieve-synchronously
;; 			 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;; 		(goto-char (point-max))
;; 		(eval-print-last-sexp)))

;; ;; el-get
;; (el-get-bundle auto-complete)
;; (el-get-bundle evil)
;; (el-get-bundle helm)
;; (el-get-bundle tarao/el-get-lock)


;; (url-retrieve
;;  "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el"
;;  (lambda (s)
;; 	 (goto-char (point-max))
;; 	 (eval-print-last-sexp)))
;; ;; `(setq my-packages
;; ;; 			 ',(mapcar #'el-get-as-symbol
;; ;; 								 (el-get-list-package-names-with-status "installed")))
;; ;; (el-get 'sync my-packages)
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; (unless (require 'el-get nil 'noerror)
;; 	(with-current-buffer
;; 			(url-retrieve-synchronously
;; 			 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;; 		(goto-char (point-max))
;; 		(eval-print-last-sexp)))

;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;; (el-get 'sync)
