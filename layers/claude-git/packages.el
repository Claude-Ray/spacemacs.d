;;; packages.el --- claude-git layer packages file for Spacemacs.
;;
;; Copyright (c) 2021 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-git-packages
  '(
    git-link
    ))

(defun claude-git/post-init-git-link ()
  (with-eval-after-load 'git-link
    (add-to-list 'git-link-remote-alist
                 '("git.guahao-inc.com" git-link-gitlab))
    (add-to-list 'git-link-commit-remote-alist
                 '("git.guahao-inc.com" git-link-commit-github))))
