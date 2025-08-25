;;; funcs.el --- claude-git layer functions file for Spacemacs.
;;
;; Copyright (c) 2021 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude-git/yadm-magit-status ()
  "Manage yadm files with Magit and TRAMP."
  (interactive)
  (require 'tramp)
  (magit-status "/yadm::"))

(defun claude-git/gitlab-init-feature ()
  "Create and checkout new branches with feature name by the prompt."
  (interactive)
  (require 'magit-process)
  (let ((text (string-trim (read-from-minibuffer "Feature name: "))))
    (when (zerop (length text))
      (user-error "Empty input"))
    (let* ((today (format-time-string "%Y%m%d"))
           (user-name (magit-git-string "config" "user.name"))
           ;; TEMP: Use a separate master branch for isolation
           (start-point
            (if (string-prefix-p "aliyun_" text) "master_aliyun" "master"))
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

(defun claude-git/gitlab-merge-request ()
  "Open merge request with current develop branch in browse-url."
  (interactive)
  (require 'browse-at-remote)
  (require 'magit-branch)
  (let* ((remote-ref (browse-at-remote--remote-ref))
         (remote (car remote-ref))
         (target-repo (browse-at-remote--get-url-from-remote remote))
         (repo-url (plist-get target-repo :url))
         (source (magit-get-current-branch))
         (is-dev-branch (string-prefix-p "dev_" source))
         (is-master-branch (string-equal source "master"))
         (target (if is-dev-branch
                     (replace-regexp-in-string "^\\(dev_\\)" "test_" source)
                   "master"))
         ;; NOTE: Distinguish between CodeBase and Gitlab by branch name
         (is-feat-branch (string-prefix-p "feat" source))
         (hyphen (if is-feat-branch "" "/-"))
         (url (concat repo-url hyphen "/merge_requests/new"
                      (when (not is-master-branch)
                        (concat "?merge_request[source_branch]=" source
                                "&merge_request[target_branch]=" target)))))
    (browse-url url)))

(defun claude-git/gitlab-review-dashboard ()
  "Review merge requests in browser."
  (interactive)
  (require 'auth-source)
  (dolist (p (auth-source-search :user "gitlab-host"
                                 :require '(:host :secret)))
    (let* ((host (plist-get p :host))
           (user (plist-get p :secret))
           (gitlab-host (if (functionp host) (funcall host) host))
           (gitlab-user (if (functionp user) (funcall user) user))
           (url (concat "https://"
                        gitlab-host
                        "/dashboard/merge_requests?assignee_username="
                        gitlab-user)))
      (browse-url url))))

(defun claude-git/magit-commit-without-diff (&optional args)
  "Don't show the relevant diff while committing."
  (interactive)
  (advice-add 'magit-commit-diff
              :around #'claude-git//magit-commit-diff)
  (magit-commit-create args))

(defun claude-git//magit-commit-diff (func &rest args)
  "One-time advice around `magit-commit-diff'."
  (let ((magit-commit-show-diff nil))
    (apply func args))
  (advice-remove 'magit-commit-diff #'claude-git//magit-commit-diff))

(defvar claude--magit-section-max-len 10
  "Truncate the children of magit-section.")

(defun claude-git//magit-find-file-noselect-advice (func rev &rest args)
  "Ignore \"{index}\" REV by replacing it with \"{worktree}\"."
  (let ((rev (if (equal rev "{index}") "{worktree}" rev)))
    (apply func rev args)))

(defun claude-git//magit-section-show-advice (section)
  "Hide the oversized children of the current section."
  (when (oref section hidden)
    (when-let ((children (oref section children))
               (len claude--magit-section-max-len))
      (dolist (child (-slice children len))
        (oset child hidden t)))))

(defun claude-git/magit-section-show-full (section)
  "Show the full children of the current section.

Alternative to `magit-section-cycle-diffs'."
  (interactive (list (magit-current-section)))
  (let ((children (oref section children))
        (claude--magit-section-max-len nil))
    (dolist (child children)
      (oset child hidden nil))
    (magit-section-show section)))
