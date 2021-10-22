;;; funcs.el --- claude-chinese layer functions file for Spacemacs.
;;
;; Copyright (c) 2021 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude-chinese//rime-candidate-num-format (num select-labels)
  (if select-labels
      (format "%s." (nth (1- num) select-labels))
    (format "%d." num)))

(defun claude-chinese//gts-display-buffer (buffer)
  "Advice after `gts-buffer-display-or-focus-buffer'."
  (with-current-buffer buffer
    ;; Avoid `gts-buffer-set-key' changing *Help* buffers
    (local-unset-key (kbd "q"))
    (local-unset-key (kbd "C-g"))
    (help-mode)))
