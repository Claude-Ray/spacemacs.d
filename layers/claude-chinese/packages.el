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
    (defun rime--build-candidate-content ()
      "Custom candidate_format."
      (let* ((context (rime-lib-get-context))
             (candidates (alist-get 'candidates (alist-get 'menu context)))
             (menu (alist-get 'menu context))
             (highlighted-candidate-index (alist-get 'highlighted-candidate-index menu))
             (idx 1)
             (result ""))
        (when (and (rime--has-composition context) candidates)
          (dolist (c candidates)
            (let* ((curr (equal (1- idx) highlighted-candidate-index))
                   (candidates-text (concat
                                     (propertize
                                      (format "%d " idx)
                                      'face
                                      'rime-candidate-num-face)
                                     (if curr
                                         (propertize (car c) 'face 'rime-highlight-candidate-face)
                                       (propertize (car c) 'face 'rime-default-face))
                                     (if-let (comment (cdr c))
                                         (propertize (format " %s" comment) 'face 'rime-comment-face)
                                       ""))))
              (setq result (concat result
                                   candidates-text
                                   (rime--candidate-separator-char))))
            (setq idx (1+ idx))))
        result))
    (set-face-attribute 'rime-highlight-candidate-face nil :foreground "DodgerBlue")
    (setq default-input-method "rime"
          rime-librime-root "~/.emacs.d/librime/dist"
          rime-show-preedit 'inline
          rime-posframe-style 'simple
          rime-posframe-properties
          (list :left-fringe 0
                :right-fringe 0)
          rime-show-candidate 'posframe
          rime-user-data-dir (expand-file-name "~/Library/Rime/emacs/"))))
