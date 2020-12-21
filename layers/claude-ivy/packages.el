;;; packages.el --- claude-ivy Layer packages file for Spacemacs
;;
;; Copyright (c) 2019 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-ivy-packages
  '(
    ivy
    ivy-rich
    ))

(defun claude-ivy/post-init-ivy ()
  ;; Remove default "^"
  (setq ivy-initial-inputs-alist '((Man-completion-table . "^")
                                   (woman . "^")))

  ;; Fix counsel-find-file TAB completion by replacing ivy-partial-or-done
  ;; with ivy-alt-done. (https://github.com/syl20bnr/spacemacs/issues/7516)
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done))

(defun claude-ivy/init-ivy-rich ()
  (use-package ivy-rich
    ;; if `counsel' loads after `ivy-rich', it overrides some of `ivy-rich''s transformers
    :after counsel
    :init
    (progn
      (setq ivy-rich-path-style 'abbrev
            ivy-virtual-abbreviate 'full))
    :config
    ;; Fix lag in ivy-switch-buffer when ivy-rich-mode is enabled
    (advice-add 'ivy-rich--ivy-switch-buffer-transformer
                :around 'claude-ivy//ivy-rich-cache-lookup)
    (advice-add 'ivy-switch-buffer
                :after 'claude-ivy//ivy-rich-cache-rebuild-trigger)
    (ivy-rich-mode)))
