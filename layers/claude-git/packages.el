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
    forge
    git-link
    magit
    ))

(defun claude-git/pre-init-forge ()
  (setq forge-add-default-sections nil
        forge-add-default-bindings nil))

(defun claude-git/post-init-git-link ()
  (with-eval-after-load 'git-link
    (add-to-list 'git-link-remote-alist
                 '("git.guahao-inc.com" git-link-gitlab))
    (add-to-list 'git-link-commit-remote-alist
                 '("git.guahao-inc.com" git-link-commit-github))))

(defun claude-git/post-init-magit ()
  (setq magit-revision-insert-related-refs nil)
  (advice-add 'magit-section-show
              :before #'claude-git//magit-section-show-advice)
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-commit "c"
      '("d" "Commit without diff" claude-git/magit-commit-without-diff))))
