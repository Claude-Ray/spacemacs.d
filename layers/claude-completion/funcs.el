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

(defvar claude--company-advice-p t
  "If non nil then company-advice is enabled.")

(defun claude-completion//company-advice (func &rest args)
  "Disable `company-mode' for specific modes.

Adding advice around company-mode, since `spacemacs|disable-company'
and `:disabled-for' are not working as expected.
https://github.com/syl20bnr/spacemacs/issues/14835"
  (unless (and claude--company-advice-p
           (or (eq major-mode 'markdown-mode)
               (eq major-mode 'org-mode)))
    (apply func args)))

(defun claude-completion/toggle-company-mode ()
  "Toggle `company-mode' in current buffer regardless of whether
the company-advice is enabled or not."
  (interactive)
  (let ((claude--company-advice-p nil))
    (if company-mode
        (company-mode -1)
      (company-mode))))

(defun claude-completion//smartparens-advice ()
  "Things only work after dotspacemacs/user-config."
  (with-eval-after-load 'smartparens
    ;; FIXME: Not sure if smartparens will mess snippets
    ;; expanded by `hippie-expand`.
    (remove-hook 'yas-before-expand-snippet-hook
                 #'spacemacs//smartparens-disable-before-expand-snippet)
    (remove-hook 'yas-after-exit-snippet-hook
                 #'spacemacs//smartparens-restore-after-exit-snippet)))
