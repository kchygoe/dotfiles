;;; init.el -*- lexical-binding: t; -*-
;;
;; native-comp
;; (setq comp-speed 2)

;; Load the heart of Doom Emacs
(setq doom-emacs-directory "~/src/github.com/hlissner/doom-emacs/")
(load (concat doom-emacs-directory "core/core")
      nil 'nomessage)

(unless (boundp 'doom-version)
  (load (concat (file-name-directory load-file-name) "early-init")
        nil t))

;; And let 'er rip!
(doom-initialize)
;; (if noninteractive
;;     (doom-initialize-packages)
;;   (doom-initialize-core)
;;   (doom-initialize-modules))
