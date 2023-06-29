;;; funcs.el --- claude-tools layer functions file for Spacemacs.
;;
;; Copyright (c) 2022 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude-tools//restclient-clear-response (raw-http-response-buffer
                                                target-buffer-name
                                                same-name)
  "Kill the target-buffer before `restclient-decode-response',
to ensure restclient will always generate a new response buffer
with the same name."
  (when (and same-name
             (get-buffer target-buffer-name))
    (kill-buffer target-buffer-name)))

(defun claude-tools/dunstctl-close-all ()
  (interactive)
  (shell-command "dunstctl close-all"))

(defun claude-tools/dunstctl-history-pop ()
  (interactive)
  (shell-command "dunstctl history-pop"))

(defun claude-tools/leetcode-show-current-problem-in-browser ()
  "Open the current problem in browser."
  (interactive)
  (let* ((title (nth 0 (split-string (buffer-name) "\\.")))
         (url (concat "https://leetcode.com/problems/" title)))
    (browse-url url)))
