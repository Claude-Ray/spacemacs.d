;;; packages.el --- claude-chinese Layer packages file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-chinese-packages
  '(
    rime
    ))

(defun claude-chinese/init-rime ()
  (use-package rime
    :config
    (setq default-input-method "rime"
          rime-librime-root "~/.emacs.d/librime/dist"
          rime-show-preedit 'inline
          rime-posframe-style 'simple
          rime-posframe-properties
          (list :left-fringe 0
                :right-fringe 0)
          rime-show-candidate 'posframe
          rime-user-data-dir (expand-file-name "~/Library/Rime/emacs/"))))
