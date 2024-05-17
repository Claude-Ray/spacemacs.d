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

(defun claude-chinese//gt-display-buffer (buffer render translator)
  "Advice after `gt-buffer-render-output'."
  (with-current-buffer buffer
    (help-mode)))

(defun claude-chinese/gt-to-translate ()
  "Do the translate, then exit visual state."
  (interactive)
  (let ((buffer (current-buffer)))
    (gt-do-translate)
    (evil-exit-visual-state nil buffer)))

(defun claude-chinese/gt-prompt-translate ()
  "Do the translate with `gt-prompt-picker'."
  (interactive)
  (gt-start (gt-translator
             :taker (gt-taker :prompt t)
             :engines (list (gt-google-engine)
                            (gt-google-rpc-engine)
                            (gt-bing-engine))
             :render (gt-buffer-render))))
