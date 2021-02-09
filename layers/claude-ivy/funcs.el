;;; funcs.el --- claude-ivy layer functions file for Spacemacs.
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; https://github.com/Yevgnen//ivy-rich/issues/87
(defvar claude--ivy-rich-cache (make-hash-table :test 'equal)
  "Cache for ivy-rich transformations.")

(defun claude-ivy//ivy-rich-cache-lookup (func candidate)
  (let ((result (gethash candidate claude--ivy-rich-cache)))
    (unless result
      (setq result (funcall func candidate))
      (puthash candidate result claude--ivy-rich-cache))
    result))

(defun claude-ivy//ivy-rich-cache-rebuild ()
  (mapc (lambda (buffer)
          (ivy-rich--ivy-switch-buffer-transformer (buffer-name buffer)))
        (buffer-list)))

(defun claude-ivy//ivy-rich-cache-rebuild-trigger ()
  (clrhash claude--ivy-rich-cache)
  (run-with-idle-timer 1 nil #'claude-ivy//ivy-rich-cache-rebuild))
