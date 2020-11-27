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
    vterm
    ))

(defun claude-term/post-init-vterm ()
  ;; Open vterm in insert state
  (evil-set-initial-state 'vterm-mode 'insert)

  ;; Modeline serves no purpose in vterm
  (add-hook 'vterm-mode-hook #'doom-themes-hide-modeline)

  (add-hook 'vterm-exit-functions #'claude-term//vterm-kill-buffer-on-exit)

  ;; Enable some parts of terminal shortcuts in evil insert state
  (with-eval-after-load 'vterm
    (evil-define-key 'insert vterm-mode-map
      (kbd "C-\\") 'toggle-input-method
      (kbd "C-a") 'vterm-send-C-a
      (kbd "C-b") 'vterm-send-C-b
      (kbd "C-c") 'vterm-send-C-c
      (kbd "C-d") 'vterm-send-C-d
      (kbd "C-e") 'vterm-send-C-e
      (kbd "C-f") 'vterm-send-C-f
      (kbd "C-g") 'vterm-send-C-g
      (kbd "C-k") 'vterm-send-C-k
      (kbd "C-l") 'vterm-send-C-l
      (kbd "C-n") 'vterm-send-C-n
      (kbd "C-p") 'vterm-send-C-p
      (kbd "C-r") 'vterm-send-C-r
      (kbd "C-s") 'vterm-send-C-s
      (kbd "C-t") 'vterm-send-C-t
      (kbd "C-u") 'vterm-send-C-u)))
