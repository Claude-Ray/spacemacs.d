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
  (let ((current-point (point)))
    (org-previous-block arg block-regexp)
    (org-indent-block)
    (goto-char current-point)))

(defun claude-org//org-cycle-advice (func &optional arg)
  "Advice around `org-cycle'."
  (if (and (not (org-at-block-p))
           (org-in-src-block-p))
      (claude-org/org-indent-block arg)
    (funcall func arg)))

(defun claude-org//org-refile-advice (func &rest args)
  "Advice around `org-refile'."
  (if (or (<= (or (org-current-level) 0) 1)
          (not (derived-mode-p 'org-mode)))
      (apply func args)
    (let ((up-heading-pos (save-excursion (outline-up-heading 1 t) (point)))
          target-file target-pos region-start region-end)
      (defun claude-org//update-statistics-after-kill
          (fn &optional prompt default-buffer new-nodes)
        (setq it (funcall fn prompt default-buffer new-nodes))
	      (setq target-file (nth 1 it)
		          target-pos (nth 3 it))
        it)
      (defun claude-org//get-region-markers (fn beg end)
        (setq region-start beg
              region-end end)
        (funcall fn beg end))
      (save-excursion
        (advice-add 'org-refile-get-location
                    :around #'claude-org//update-statistics-after-kill)
        (advice-add 'org-save-markers-in-region
                    :around #'claude-org//get-region-markers)
        (apply func args)
        (advice-remove 'org-refile-get-location
                       #'claude-org//update-statistics-after-kill)
        (advice-remove 'org-save-markers-in-region
                       #'claude-org//get-region-markers)
        (when (and (equal (buffer-file-name) target-file)
                   (<= target-pos up-heading-pos))
          (setq up-heading-pos (+ up-heading-pos
                                  (- region-end region-start))))
        (goto-char up-heading-pos)
        (org-update-statistics-cookies nil)))))
