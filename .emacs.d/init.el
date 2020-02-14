;;; init.el --- Global Initialization for local emacs
;; Author: Koichi Yoshigoe <koichi.yoshigoe@gmail.com>
;; Ver. 0.1.1
;;; Commentary:
;; emacs and org-mode is fun

;;; Code:

(require 'org)
(org-babel-load-file
 (expand-file-name "init.org" user-emacs-directory))

(provide 'init)

;;;
;;; init.el ends here
;;;

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
