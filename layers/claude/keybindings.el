;;; layers.el --- claude layer keybindings file for Spacemacs.
;;
;; Copyright (c) 2019 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
(define-key evil-insert-state-map (kbd "s-s") 'claude-edit/save-and-evil-exit-insert-state)

(when (spacemacs/window-system-is-mac)
  (global-set-key (kbd "s-q") (if (daemonp) 'delete-frame 'save-buffers-kill-terminal))
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-W") 'delete-frame)
  (global-set-key (kbd "s-n") 'make-frame)
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-Z") 'undo-tree-redo))

(when (display-graphic-p)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-c") 'evil-yank)
  (global-set-key (kbd "s-=") 'spacemacs/scale-up-font)
  (global-set-key (kbd "s--") 'spacemacs/scale-down-font)
  (global-set-key (kbd "s-0") 'spacemacs/reset-font-size)
  (global-set-key (kbd "s-/") 'comment-line)
  (global-set-key (kbd "s-s") 'save-buffer))

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd ",i") 'claude/gitlab-init-feature)
  (define-key magit-mode-map (kbd ",m") 'claude/gitlab-merge-request))
