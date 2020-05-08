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

  ;; Enable some parts of terminal shortcuts in evil insert state
  (with-eval-after-load 'vterm
    (evil-define-key 'insert vterm-mode-map (kbd "C-a") 'vterm-send-C-a)
    (evil-define-key 'insert vterm-mode-map (kbd "C-b") 'vterm-send-C-b)
    (evil-define-key 'insert vterm-mode-map (kbd "C-c") 'vterm-send-C-c)
    (evil-define-key 'insert vterm-mode-map (kbd "C-d") 'vterm-send-C-d)
    (evil-define-key 'insert vterm-mode-map (kbd "C-e") 'vterm-send-C-e)
    (evil-define-key 'insert vterm-mode-map (kbd "C-f") 'vterm-send-C-f)
    (evil-define-key 'insert vterm-mode-map (kbd "C-g") 'vterm-send-C-g)
    (evil-define-key 'insert vterm-mode-map (kbd "C-k") 'vterm-send-C-k)
    (evil-define-key 'insert vterm-mode-map (kbd "C-l") 'vterm-send-C-l)
    (evil-define-key 'insert vterm-mode-map (kbd "C-n") 'vterm-send-C-n)
    (evil-define-key 'insert vterm-mode-map (kbd "C-p") 'vterm-send-C-p)
    (evil-define-key 'insert vterm-mode-map (kbd "C-r") 'vterm-send-C-r)
    (evil-define-key 'insert vterm-mode-map (kbd "C-s") 'vterm-send-C-s)
    (evil-define-key 'insert vterm-mode-map (kbd "C-t") 'vterm-send-C-t)
    (evil-define-key 'insert vterm-mode-map (kbd "C-u") 'vterm-send-C-u)))
