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
