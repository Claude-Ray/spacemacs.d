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
(define-key evil-insert-state-map (kbd "M-i") 'yas-insert-snippet)
(define-key evil-insert-state-map (kbd "s-s") 'claude-edit/save-and-evil-exit-insert-state)

(with-eval-after-load 'company
  (let ((map company-active-map))
    (define-key map (kbd "C-n") 'company-select-next)
    (define-key map (kbd "C-p") 'company-select-previous)
    (define-key map (kbd "C-d") 'company-next-page)
    (define-key map (kbd "C-u") 'company-previous-page)
    (define-key map (kbd "C-h") 'company-show-doc-buffer)
    ))

;; (define-key key-translation-map (kbd "C-n") (kbd "C-j"))
;; (define-key key-translation-map (kbd "C-p") (kbd "C-k"))

(when (spacemacs/window-system-is-mac)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-c") 'evil-yank)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-q") (if (daemonp) 'delete-frame 'save-buffers-kill-terminal))
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-W") 'delete-frame)
  (global-set-key (kbd "s-n") 'make-frame)
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-Z") 'undo-tree-redo))

(when (display-graphic-p)
  (global-set-key (kbd "s-=") 'spacemacs/scale-up-font)
  (global-set-key (kbd "s--") 'spacemacs/scale-down-font)
  (global-set-key (kbd "s-0") 'spacemacs/reset-font-size)
  (global-set-key (kbd "s-/") 'comment-line)
  (global-set-key (kbd "s-s") 'save-buffer))
