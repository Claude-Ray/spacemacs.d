;;; funcs.el --- claude-prog Layer functions File for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude-prog//js2-mode-hook ()
  (progn
    (define-key js2-mode-map "@" 'js-doc-insert-tag)
    (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "i" 'js-doc-insert-function-doc)
    (setq mode-name "JS2")
    (setq forward-sexp-function nil)
    (set (make-local-variable 'semantic-mode) nil)))

(defun claude-prog//kill-compilation-hook ()
  "Kill *complication* buffer and window automatically."
  (let ((buffer (compilation-find-buffer)))
    (delete-window (get-buffer-window buffer))
    (kill-buffer buffer)))

(defun claude-prog/quick-run ()
  "Compile the program including the current buffer."
  (interactive)
  (save-buffer)
  (cond ((or (eq major-mode 'js-mode)
            (eq major-mode 'js2-mode))
         (compile (concat "node " (file-relative-name buffer-file-name)))))
  (add-hook 'kill-buffer-hook 'claude-prog//kill-compilation-hook nil t))
