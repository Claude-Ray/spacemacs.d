;;; funcs.el --- claude-term Layer functions file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude-term//projectile-shell-pop (func &rest args)
  (let ((shell-pop-autocd-to-working-dir t))
    (apply func args)))

(defun claude-term//hide-modeline-on-entry ()
  "Hide modeline in terminal."
  (setq mode-line-format nil))

(defun claude-term//kill-buffer-on-exit (buffer &optional event)
  "Kill terminal buffer when `exit'."
  (when buffer
    (kill-buffer buffer)))
