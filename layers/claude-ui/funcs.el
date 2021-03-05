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

(defun claude-ui//larger-buffer-face ()
  "Sets a larger font in current buffer"
  (setq-local buffer-face-mode-face '(:height 160))
  (buffer-face-mode))

(defun claude-ui//realign-ignore-window-p (window)
  "Check if WINDOW needs to run `realign-windows'."
  (let* ((buffer (window-buffer window))
         (buffname (string-trim (buffer-name buffer))))
    (or (equal buffname "*spacemacs*")
        (equal buffname "*rime-posframe*")
        (equal buffname "*flycheck-posframe-buffer*")
        (equal buffname "*Ediff Control Panel*")
        (equal (with-current-buffer buffer major-mode) 'telega-chat-mode)
        (equal (with-current-buffer buffer major-mode) 'mu4e-view-mode)
        (equal (with-current-buffer buffer major-mode) 'mu4e-compose-mode))))

(defun claude-ui//realign-need-padding-p (window)
  "Check if left padding is needed for given WINDOW.

This function can be used to update the window-margins dynamically."
  (let* ((buffer (window-buffer window)))
    (and (with-current-buffer buffer (not (bound-and-true-p org-present-mode)))
         ;; No padding in narrow frame
         (and (numberp split-width-threshold)
              (> (frame-width) split-width-threshold)))))

(defun claude-ui//realign-turn-on ()
  "Advice after `realign-turn-on'."
  (add-hook 'org-present-mode-hook #'realign-windows)
  ;; The quit-hooks will run before quitting org-present-mode
  (add-hook 'org-present-mode-quit-hook
            (lambda (&optional frame)
              (let ((org-present-mode nil))
                (realign-windows)))))

(defun claude-ui//realign-turn-off ()
  "Advice after `realign-turn-off'."
  (remove-hook 'org-present-mode-hook #'realign-windows)
  (remove-hook 'org-present-mode-quit-hook #'realign-windows))

(defun claude-ui//theme-enabled-p (theme)
  "Return t if theme is currently loaded."
  (equal (spacemacs//get-theme-name theme) spacemacs--cur-theme))

(defun claude-ui/toggle-undecorated (&optional frame)
  "Toggle whether or not the selected frame is undecorated."
  (interactive)
  (set-frame-parameter
   frame 'undecorated
   (not (frame-parameter nil 'undecorated))))
