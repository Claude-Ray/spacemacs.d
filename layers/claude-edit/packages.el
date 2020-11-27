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
    evil-pinyin
    evil-snipe
    undo-tree
    smartparens
    ))

(defun claude-edit/init-evil-pinyin ()
  (use-package evil-pinyin
    :init (setq evil-pinyin-scheme 'simplified-xiaohe-all)
    :config (global-evil-pinyin-mode)))

(defun claude-edit/post-init-evil-snipe ()
  (setq evil-snipe-scope 'visible)
  (evil-define-key 'visual evil-snipe-local-mode-map
    "z" 'evil-snipe-s
    "Z" 'evil-snipe-S))

(defun claude-edit/post-init-undo-tree ()
  ;; undo-in-region is known to cause undo history corruption
  (setq undo-tree-enable-undo-in-region nil))

(defun claude-edit/post-init-smartparens()
  ;; Typing a single quote instead of \'\' in smartparens-mode.
  (setq sp-escape-quotes-after-insert nil))
