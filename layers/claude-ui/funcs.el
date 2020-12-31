;;; funcs.el --- claude-ui layer functions file for Spacemacs.
;;
;; Copyright (c) 2021 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude-ui//realign-ignore-window-p (window)
  (let* ((buffer (window-buffer window))
         (buffname (string-trim (buffer-name buffer))))
    (or (equal buffname "*spacemacs*")
        (equal buffname "*flycheck-posframe-buffer*")
        (equal buffname "*Ediff Control Panel*")
        (equal (with-current-buffer buffer major-mode) 'mu4e-view-mode)
        (equal (with-current-buffer buffer major-mode) 'mu4e-compose-mode))))
