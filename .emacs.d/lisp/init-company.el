;;; init-company.el -- company
;;; Commentary:
;;; Code:
(use-package company
  :bind (
         :map company-active-map
         ( "M-n" . nil)
         ( "M-p" . nil)
         ( "C-n" . company-select-next)
         ( "C-p" . company-select-previous)
         ( "C-h" . nil)
         ( "C-s" . company-filter-candidates)
         ( "C-i" . company-complete-selection)
         ( "<tab>" . company-complete-common-or-cycle)
         ( "M-d" . company-show-doc-buffer)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         :map emacs-lisp-mode-map
         ("C-M-i" . company-complete))
  :config
  (global-company-mode +1)
  (company-quickhelp-mode +1)
  (custom-set-variables '(company-idle-delay nil))
  (setq company-transformers '(company-sort-by-backend-importance))
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  ;; (setq company-dabbrev-downcase nil)
  ;; like auto-complete
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40"))

;; company with yasnippet
(defvar company-backends)
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(provide 'init-company)
;;; init-company.el ends here
