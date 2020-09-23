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

(setq auto-window-vscroll nil)

(when (display-graphic-p)
  ;; Ask for confirmation when leaving Emacs.
  (setq confirm-kill-emacs '(lambda (prompt) (y-or-n-p-with-timeout prompt 10 "y"))))

;; eww
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|127.*\\|192.*\\|10.*\\)")
        ("http" . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890")))
