;;; config.el --- claude Layer configuration file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq user-full-name "Claude Ray"
      user-mail-address "yunleiqi@gmail.com")

;; Don't generate backups or lockfiles while auto-save is enabled
(setq create-lockfiles nil
      make-backup-files nil
      ;; Don't clobber links
      backup-by-copying t)

;; Reduce cursor lag
(setq auto-window-vscroll nil)

;; Don't create a new warning buffer at bottom
(setq warning-display-at-bottom nil)

(setq calendar-latitude 30.3
      calendar-longitude 120.0)

(when (display-graphic-p)
  ;; Ask for confirmation when leaving Emacs.
  (setq confirm-kill-emacs '(lambda (prompt) (y-or-n-p-with-timeout prompt 10 "y"))))

;; eww
(when (executable-find "clash")
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|127.*\\|192.*\\|10.*\\)")
          ("http" . "127.0.0.1:7890")
          ("https" . "127.0.0.1:7890"))))

(add-hook 'spacemacs-scratch-mode-hook #'claude//spacemacs-scratch-mode-hook)

(setq spacemacs-keep-legacy-current-buffer-delete-bindings t)

(advice-add 'make-auto-save-file-name
            :around #'claude//shorten-auto-save-file-name)
