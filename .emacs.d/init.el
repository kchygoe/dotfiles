;;; init.el --- Global Initialization for local emacs
;; Author: Koichi Yoshigoe <koichi.yoshigoe@gmail.com>
;; Ver. 0.1.1
;;; Commentary:
;; emacs and org-mode is fun

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "initial.org" user-emacs-directory))

(provide 'init)

;;;
;;; init.el ends here
;;;

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
