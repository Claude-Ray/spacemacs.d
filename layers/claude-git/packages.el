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
    magit
    ))

(defun claude-git/post-init-git-link ()
  (with-eval-after-load 'git-link
    (add-to-list 'git-link-remote-alist
                 '("git.guahao-inc.com" git-link-gitlab))
    (add-to-list 'git-link-commit-remote-alist
                 '("git.guahao-inc.com" git-link-commit-github))))

(defun claude-git/post-init-magit ()
  (setq magit-commit-show-diff nil
        magit-diff-refine-hunk t
        magit-revision-insert-related-refs nil)
  (with-eval-after-load 'magit
    ;; Replaced by `claude-git//magit-commit-diff'
    (remove-hook 'server-switch-hook 'magit-commit-diff)
    (transient-append-suffix 'magit-commit "c"
      '("d" "Commit with diff" claude-git/magit-commit-with-diff))))
