;;; packages.el --- claude-edit Layer packages file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-edit-packages
  '(
    undo-tree
    smartparens
    ))

(defun claude-edit/post-init-undo-tree ()
  ;; undo-in-region is known to cause undo history corruption
  (setq undo-tree-enable-undo-in-region nil))

(defun claude-edit/post-init-smartparens()
  ;; Disabled for typing a single quote instead of \'\' in smartparens-mode.
  (setq sp-escape-quotes-after-insert nil))
