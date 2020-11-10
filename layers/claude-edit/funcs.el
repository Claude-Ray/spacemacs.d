;;; funcs.el --- claude-edit layer functions file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude-edit/save-and-evil-exit-insert-state ()
  "Save current buffer, then exit evil insert state."
  (interactive)
  (when (evil-insert-state-p)
    (evil-normal-state))
  (save-buffer))
