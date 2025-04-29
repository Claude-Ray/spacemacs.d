;;; funcs.el --- claude-ai Layer functions file for Spacemacs
;;
;; Copyright (c) 2025 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude-ai/cancel-on-electric-indent-chars (arg)
  "Cancel Copilot completion eagerly when electric-indent-mode is triggered.

ARG is the prefix argument passed to `self-insert-command'."
  (interactive "p")
  ;; If the Copilot overlay is visible and the last command event is in
  ;; `electric-indent-chars`, clear the overlay without treating it as a rejection.
  (when (and (copilot--overlay-visible)
             (memq last-command-event electric-indent-chars))
    (delete-overlay copilot--overlay)
    (setq copilot--real-posn nil))
  ;; Proceed with the default self-insert behavior.
  (self-insert-command arg))

(defun claude-ai//override-electric-keys ()
  "Override electric keys for Copilot in the current buffer."
  (dolist (char electric-indent-chars)
    (define-key copilot-completion-map (vector char)
                #'claude-ai/cancel-on-electric-indent-chars)))

(defun claude-ai//setup-electric-indent-overrides ()
  "Set up electric key overrides for Copilot in the current major mode."
  (add-hook 'after-change-major-mode-hook
            #'claude-ai//override-electric-keys nil 'local))
