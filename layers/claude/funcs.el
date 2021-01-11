;;; funcs.el --- claude Layer functions File for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude/yadm-magit-status ()
  "Manage yadm files with Magit and TRAMP."
  (interactive)
  (require 'tramp)
  (magit-status "/yadm::"))

(defun claude/gitlab-init-feature ()
  "Create and checkout new branches with feature name by the prompt."
  (interactive)
  (require 'magit-process)
  (let ((text (string-trim (read-from-minibuffer "Feature name: "))))
    (when (zerop (length text))
      (user-error "Empty input"))
    (let* ((today (format-time-string "%Y%m%d"))
           (user-name (magit-git-string "config" "user.name"))
           (start-point "master")
           (branch-suffix (format "%s_%s_%s" text user-name today))
           (dev-branch (concat "dev_" branch-suffix))
           (test-branch (concat "test_" branch-suffix)))
      (magit-call-git "stash")
      (magit-call-git "checkout" start-point)
      (magit-call-git "pull")
      (magit-call-git "branch" test-branch)
      ;; Init remote test-branch for the convenience in later MergeRequest
      (magit-call-git "push" "origin" test-branch)
      (message "Created remote branch '%s'." test-branch)
      (magit-call-git "checkout" "-b" dev-branch)
      (message "Switched to branch '%s'" dev-branch))))

(defun claude/gitlab-merge-request ()
  "Open merge request with current develop branch in browse-url."
  (interactive)
  (require 'browse-at-remote)
  (require 'magit-branch)
  (let* ((remote-ref (browse-at-remote--remote-ref))
         (remote (car remote-ref))
         (target-repo (browse-at-remote--get-url-from-remote remote))
         (repo-url (cdr target-repo))
         (source (magit-get-current-branch))
         (is-dev-branch (string-prefix-p "dev_" source))
         (is-master-branch (string-equal source "master"))
         (target (if is-dev-branch
                     (replace-regexp-in-string "^\\(dev_\\)" "test_" source)
                   "master"))
         (url (concat repo-url "/-/merge_requests/new"
                      (when (not is-master-branch)
                        (concat "?merge_request[source_branch]=" source
                                "&merge_request[target_branch]=" target)))))
    (browse-url url)))
