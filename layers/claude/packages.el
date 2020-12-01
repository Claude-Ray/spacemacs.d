;;; packages.el --- claude Layer packages file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-packages
  '(
    memory-usage
    (recentf :location built-in)
    tramp
    which-key
    ))

(defun claude/init-memory-usage ()
  (use-package memory-usage
    :defer t
    :config
    (advice-add 'memory-usage :after #'help-mode)))

(defun claude/post-init-recentf ()
  (when (spacemacs/window-system-is-mac)
    ;; Disabled for "...emacs.d/.cache/recentf locked by xxx".
    ;; You can call recentf-save-list manually to save recent files.
    (cancel-timer recentf-auto-save-timer)))

(defun claude/init-tramp ()
  (use-package tramp
    :defer t
    :config
    (add-to-list 'tramp-methods
                 '("yadm"
                   (tramp-login-program "yadm")
                   (tramp-login-args (("enter")))
                   (tramp-login-env (("SHELL") ("/bin/bash")))
                   (tramp-remote-shell "/bin/sh")
                   (tramp-remote-shell-args ("-c"))
                   (tramp-connection-timeout 10)))))

(defun claude/post-init-which-key ()
  (define-key global-map "\C-h\C-m" 'which-key-show-major-mode))
