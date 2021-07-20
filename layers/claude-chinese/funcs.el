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

(defun claude-chinese//rime-uninit-hook-default ()
  "Uninit for command `rime-active-mode'."
  (remove-hook 'post-self-insert-hook 'rime--redisplay t)
  (rime--redisplay))

(defun claude-chinese//rime-uninit-hook-vterm ()
  "Rime finalize for vterm-mode."
  (advice-remove 'vterm--redraw 'rime--redisplay)
  (rime--redisplay))
