;;; ~/src/github.com/kchygoe/dotfiles/doom.d/bindings.el -*- lexical-binding: t; -*-
;;; Code:
(map!

 ;; general
 "<C-return>" #'other-window
 "C-c i" #'indent-region
 "C-c C-i" #'dabbrev-expand
 "C-c ;" #'comment-region
 "C-c :" #'uncomment-region
 "C-s" #'swiper
 "C-c s" #'query-replace
 "C-u" #'scroll-down
 "C-h" #'delete-backward-char
 "M-?" #'help-for-help
 "M-n" #'goto-line

;; ivy
 "C-x b" #'ivy-switch-buffer

 ;; org-mode
 "C-c l"  #'org-store-link
 "C-c a"  #'org-agenda
 ;;"C-c c"  #'org-capture

 ;; org-roam
 "C-c n l" #'org-roam
 "C-c n t" #'org-roam-today
 "C-c n f" #'org-roam-find-file
 "C-c n i" #'org-roam-insert
 "C-c n g" #'org-roam-show-graph

 ;; company
(:after company
  :map company-active-map
  "C-j"         #'company-select-next
  "C-n"         #'company-select-next
  "C-k"         #'company-select-previous
  "C-p"         #'company-select-previous
  "C-d"         #'company-show-doc-buffer
  )
)

(provide '+bindings)

;;;
;;; bind
;;;
;; (bind-key "<C-return>" 'other-window)
;; (bind-key "C-c i" 'indent-region)
;; (bind-key "C-c C-i" 'dabbrev-expand)
;; (bind-key "C-c ;" 'comment-region)
;; (bind-key "C-c :" 'uncomment-region)
;; (bind-key "C-c s" 'query-replace)
;; (bind-key "C-u" 'scroll-down)
;; (bind-key "C-h" 'delete-backward-char)
;; (bind-key "M-?" 'help-for-help)
;; (bind-key "M-n" 'goto-line)
;; (bind-key "C-c c" 'org-capture)


;;; bindings.el ends here
