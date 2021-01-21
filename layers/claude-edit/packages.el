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
    evil
    evil-mc
    evil-pinyin
    evil-snipe
    paredit
    undo-tree
    smartparens
    ))

(defun claude-edit/post-init-evil ()
  (setq evil-ex-search-vim-style-regexp t)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
  (define-key evil-insert-state-map (kbd "s-s")
    'claude-edit/save-and-evil-exit-insert-state))

(defun claude-edit/post-init-evil-mc ()
  (add-hook 'evil-mc-before-cursors-created
            #'claude-edit//evil-mc-before-created)
  (add-hook 'evil-mc-after-cursors-deleted
            #'claude-edit//evil-mc-after-deleted))

(defun claude-edit/init-evil-pinyin ()
  (use-package evil-pinyin
    :init (setq evil-pinyin-scheme 'simplified-xiaohe-all)
    :config (global-evil-pinyin-mode)))

(defun claude-edit/post-init-evil-snipe ()
  (setq evil-snipe-scope 'visible)
  (evil-define-key 'visual evil-snipe-local-mode-map
    "z" 'evil-snipe-s
    "Z" 'evil-snipe-S))

(defun claude-edit/init-paredit ()
  (use-package paredit
    :hook (emacs-lisp-mode . paredit-mode)))

(defun claude-edit/post-init-undo-tree ()
  ;; undo-in-region is known to cause undo history corruption
  (setq undo-tree-enable-undo-in-region nil))

(defun claude-edit/post-init-smartparens()
  (show-smartparens-global-mode -1)
  (with-eval-after-load 'smartparens
    (dolist (pair '("\"" "'" "`"))
      (sp-pair pair nil
               :unless '(claude-edit//sp-point-after-same-p
                         sp-point-after-word-p
                         sp-point-before-same-p
                         sp-point-before-word-p)))
    (dolist (pair '("(" "[" "{"))
      (sp-pair pair nil
               :post-handlers
               '(:add (spacemacs/smartparens-pair-newline-and-indent "RET"))
               :unless '(sp-point-before-same-p
                         sp-point-before-word-p))))
  ;; Typing a single quote instead of \'\' in smartparens-mode.
  (setq sp-escape-quotes-after-insert nil))
