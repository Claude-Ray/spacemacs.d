;;; funcs.el --- claude-org Layer functions file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude-org//verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun claude-org/org-agenda-tree-to-indirect-buffer ()
  "Show the subtree corresponding to the current entry in an indirect buffer.

This will open buffer in a side window just like `org-agenda-goto',
instead of creating a new window at the bottom."
  (interactive)
  (org-agenda-goto)
  (org-tree-to-indirect-buffer))

(defun claude-org/org-clock-schedule ()
  "Schedule a clock on the current item.

Prompt for two dates/times and insert a resolved clock."
  (interactive)
  (let* ((default-start-time (org-current-time 30))
         (start-time (org-read-date
                      t t nil nil default-start-time)))
    (org-clock-in nil start-time)
    (let* ((default-end-time (time-add
                             start-time
                             (seconds-to-time 1800)))
           (end-time (org-read-date
                      t t nil nil default-end-time)))
      (org-clock-out nil t end-time))))

(defun claude-org/org-indent-block (arg &optional block-regexp)
  "Indent the block around point."
  (interactive "p")
  (org-previous-block arg block-regexp)
  (org-indent-block))
