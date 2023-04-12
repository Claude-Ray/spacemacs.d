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

(defun claude-edit//evil-mc-save-keys (flag pre-name post-name keys)
  "Save KEYS at PRE-NAME or POST-NAME according to FLAG.

FIXME: Fix `evil-change' by adding support for pre-read-key-sequence.
https://github.com/gabesoft/evil-mc/issues/131"
  (cl-ecase flag
    (pre (evil-mc-add-command-property pre-name keys))
    (post (evil-mc-add-command-property post-name keys))
    (pre-read-key-sequence (evil-mc-add-command-property post-name keys))))

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

(defun claude-edit//electric-pair-inhibit (char)
  "Predicate to prevent insertion of a matching pair."
  (or (minibufferp)
      (claude-edit//point-before-same-p char)
      (claude-edit//point-before-word-p)
      (when (memq char '(?\" ?\' ?\`))
        (or (claude-edit//point-after-same-p char)
            (claude-edit//point-after-word-p)))))

(defun claude-edit//point-before-same-p (char)
  "Return t if point is followed by the same char, nil otherwise."
  (eq char (char-after)))

(defun claude-edit//point-after-same-p (char)
  "Return t if point is after the same char, nil otherwise."
  (and (eq char (char-before))
	     (eq char (char-before (1- (point))))))

(defun claude-edit//point-before-word-p ()
  "Return t if point is followed by a word, nil otherwise."
  ;; (eq (char-syntax (following-char)) ?w)
  (memq (char-syntax (following-char)) '(?w ?_)))

(defun claude-edit//point-after-word-p ()
  "Return t if point is after a word, nil otherwise."
	(backward-char 1)
  (memq (char-syntax (preceding-char)) '(?w ?_)))

(defun claude-edit//electric-pair-inhibit-angle-brackets ()
  "Prevent `electric-pair-mode' from matching `<' with a `>'."
  (setq-local electric-pair-inhibit-predicate
              `(lambda (c)
	               (if (char-equal c ?\<)
                     t
                   (,electric-pair-inhibit-predicate c)))))
