;;; funcs.el --- claude-term Layer functions file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude-term//vterm-kill-buffer-on-exit (buffer &optional event)
  "Kill vterm buffer when `exit'"
  (when buffer
    (kill-buffer buffer)))
