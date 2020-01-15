;;; funcs.el --- claude Layer functions File for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude/yadm-magit-status ()
  "Manage yadm files with Magit and TRAMP."
  (interactive)
  (require 'tramp)
  (let ((magit-mode-hook
         (-cons*
          (lambda () (projectile-mode -1))
          (lambda () (when (fboundp #'magit-todos-mode) (magit-todos-mode -1)))
          magit-mode-hook)))
    (setenv "SHELL" "/bin/bash")
    (magit-status "/yadm::")))
