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
    vc
    ))

(defun claude-git/pre-init-forge ()
  (setq forge-add-default-bindings nil))

(defun claude-git/post-init-git-link ()
  (with-eval-after-load 'git-link
    (require 'auth-source)
    (dolist (p (auth-source-search
                :user "gitlab-host" :require '(:host)))
      (let* ((host (plist-get p :host))
             (gitlab-host (if (functionp host) (funcall host) host)))
        (add-to-list 'git-link-remote-alist
                     `(,gitlab-host git-link-gitlab))
        (add-to-list 'git-link-commit-remote-alist
                     `(,gitlab-host git-link-commit-github))))))

(defun claude-git/post-init-magit ()
  (setq magit-revision-insert-related-refs nil)
  (advice-add 'magit-section-show
              :before #'claude-git//magit-section-show-advice)
  (advice-add 'magit-find-file-noselect
              :around #'claude-git//magit-find-file-noselect-advice)
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-commit "c"
      '("d" "Commit without diff" claude-git/magit-commit-without-diff))))

(defun claude-git/post-init-vc ()
  ;; XXX: Symbol’s value as variable is void: vc-svn-log-view-mode-map
  (with-eval-after-load 'log-view
    (require 'vc-svn)))
