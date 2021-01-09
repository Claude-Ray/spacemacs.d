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

(defun claude-edit//evil-mc-before-created ()
  "Actions to be executed before any cursors are created.

This function should be hooked to `evil-mc-before-cursors-created'."
  ;; Make \\[C-g] delete clear all multi-cursors
  ;; Use evil-define-key instead of global-set-key for better compatibility
  ;; with keyboard-quit in spacemacs-purpose.
  ;; (global-set-key [remap keyboard-quit] #'claude-edit/evil-mc-keyboard-quit)
  ;; (global-set-key [remap keyboard-quit] nil)
  (evil-define-key 'normal evil-mc-key-map
    (kbd "C-g") #'claude-edit/evil-mc-keyboard-quit))

(defun claude-edit//evil-mc-after-deleted ()
  "Actions to be executed after all cursors are deleted.

This function should be hooked to `evil-mc-after-cursors-deleted'."
  ;; No more multi-cursors, revert \\[C-g] to `keyboard-quit'
  (evil-define-key 'normal evil-mc-key-map
    (kbd "C-g") #'keyboard-quit))

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
