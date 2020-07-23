;;; init.el -*- lexical-binding: t; -*-
;;

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

(let (file-name-handler-alist)
  ;; Ensure Doom is running out of this file's directory
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Load the heart of Doom Emacs
(setq doom-emacs-directory "~/src/github.com/hlissner/doom-emacs/")
(load (concat doom-emacs-directory "core/core")
      nil 'nomessage)

;; And let 'er rip!
(doom-initialize)
;; (if noninteractive
;;     (doom-initialize-packages)
;;   (doom-initialize-core)
;;   (doom-initialize-modules))
