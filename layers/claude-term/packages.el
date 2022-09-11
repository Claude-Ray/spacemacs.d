;;; packages.el --- claude-term Layer packages file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-term-packages
  '(
    shell-pop
    vterm
    ))

(defun claude-term/post-init-shell-pop ()
  (setq shell-pop-autocd-to-working-dir nil)
  (advice-add 'spacemacs/projectile-shell-pop
              :around #'claude-term//projectile-shell-pop))

(defun claude-term/post-init-vterm ()
  ;; Open vterm in insert state
  (evil-set-initial-state 'vterm-mode 'insert)

  ;; Modeline serves no purpose in vterm
  (add-hook 'vterm-mode-hook #'claude-term//hide-modeline-on-entry)
  (add-hook 'vterm-exit-functions #'claude-term//kill-buffer-on-exit)

  ;; Enable some parts of terminal shortcuts in evil insert state
  (with-eval-after-load 'vterm
    (evil-define-key 'insert vterm-mode-map
      (kbd "C-\\") 'toggle-input-method
      (kbd "C-a") 'vterm--self-insert
      (kbd "C-b") 'vterm--self-insert
      (kbd "C-c") 'vterm--self-insert
      (kbd "C-d") 'vterm--self-insert
      (kbd "C-e") 'vterm--self-insert
      (kbd "C-f") 'vterm--self-insert
      (kbd "C-g") 'vterm--self-insert
      (kbd "C-k") 'vterm--self-insert
      (kbd "C-l") 'vterm--self-insert
      (kbd "C-n") 'vterm--self-insert
      (kbd "C-p") 'vterm--self-insert
      (kbd "C-r") 'vterm--self-insert
      (kbd "C-s") 'vterm--self-insert
      (kbd "C-t") 'vterm--self-insert
      (kbd "C-u") 'vterm--self-insert)))
