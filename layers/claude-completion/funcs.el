;;; funcs.el --- claude-completion layer functions file for Spacemacs.
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude-completion//company-advice ()
  "Things only work after dotspacemacs/user-config."
  (spacemacs|disable-company org-mode)
  (with-eval-after-load 'company
    (define-key company-active-map
      (kbd "C-n") 'company-select-next-if-tooltip-visible-or-complete-selection)))

(defun claude-completion//smartparens-advice ()
  "Things only work after dotspacemacs/user-config."
  (with-eval-after-load 'smartparens
    ;; FIXME: Not sure if smartparens will mess snippets
    ;; expanded by `hippie-expand`.
    (remove-hook 'yas-before-expand-snippet-hook
                 #'spacemacs//smartparens-disable-before-expand-snippet)
    (remove-hook 'yas-after-exit-snippet-hook
                 #'spacemacs//smartparens-restore-after-exit-snippet)))
