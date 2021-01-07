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

(defun claude-edit/evil-mc-keyboard-quit ()
  "Delete all cursors when doing keyboard-quit."
  (interactive)
  (setq this-command 'keyboard-quit)
  (if (evil-mc-has-cursors-p)
      (evil-mc-undo-all-cursors))
  (keyboard-quit))

(defun claude-edit/save-and-evil-exit-insert-state ()
  "Save current buffer, then exit evil insert state."
  (interactive)
  (when (evil-insert-state-p)
    (evil-normal-state))
  (save-buffer))

(defun claude-edit//sp-point-after-same-p (id action _context)
  "Return t if point is led by ID, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (save-excursion
      (backward-char 1)
      (sp--looking-back-p (regexp-quote id)))))
