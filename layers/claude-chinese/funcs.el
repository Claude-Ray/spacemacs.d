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
    (help-mode)))

(defun claude-chinese/gts-to-translate ()
  "Do the translate, then exit visual state."
  (interactive)
  (let ((buffer (current-buffer)))
    (gts-do-translate)
    (evil-exit-visual-state nil buffer)))

(defun claude-chinese/gts-prompt-translate ()
  "Do the translate with `gts-prompt-picker'."
  (interactive)
  (let ((gts-translate-list '(("en" "zh") ("zh" "en"))))
    (gts-translate (gts-translator
                    :picker (gts-prompt-picker)
                    :engines (list (gts-google-engine)
                                   (gts-google-rpc-engine)
                                   (gts-bing-engine))
                    :render (gts-buffer-render)))))
