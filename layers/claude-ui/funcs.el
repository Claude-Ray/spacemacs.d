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

(defun claude-ui//append-pragmatapro-prettify-symbols-alist ()
  "Append pragmatapro to prettify-symbols-alist instead of a full reset."
  (setq prettify-symbols-alist
        (append pragmatapro-prettify-symbols-alist
                prettify-symbols-alist)))

(defun claude-ui//larger-buffer-face ()
  "Sets a larger font in current buffer"
  (setq-local buffer-face-mode-face '(:height 160))
  (buffer-face-mode))

(defvar-local claude--realign-p t
  "This is used to toggle `realign-mode' padding effect buffer-local.")

(defun claude-ui//realign-ignore-window-p (window)
  "Check if WINDOW needs to run `realign-windows'."
  (let* ((buffer (window-buffer window))
         (buffname (string-trim (buffer-name buffer)))
         (mode (with-current-buffer buffer major-mode)))
    (or (equal buffname "*spacemacs*")
        (equal buffname "*rime-posframe*")
        (equal buffname "*flycheck-posframe-buffer*")
        (equal buffname "*Ediff Control Panel*")
        (equal mode 'telega-chat-mode)
        (equal mode 'mu4e-view-mode)
        (equal mode 'mu4e-compose-mode)
        (with-current-buffer buffer (bound-and-true-p writeroom-mode)))))

(defun claude-ui//realign-need-padding-p (window)
  "Check if left padding is needed for given WINDOW.

This function can be used to update the window-margins dynamically."
  (let* ((buffer (window-buffer window)))
    (and (with-current-buffer buffer
           (and (bound-and-true-p claude--realign-p)
                (not (bound-and-true-p org-present-mode))))
         ;; No padding in narrow frame
         (and (numberp split-width-threshold)
              (> (frame-width) split-width-threshold)))))

(defun claude-ui//realign-turn-on ()
  "Advice after `realign-turn-on'."
  (add-hook 'calendar-initial-window-hook #'realign-windows)
  (add-hook 'org-present-mode-hook #'realign-windows)
  (add-hook 'org-present-mode-quit-hook
            #'claude-ui//realign-org-present-quit-hook))

(defun claude-ui//realign-turn-off ()
  "Advice after `realign-turn-off'."
  (remove-hook 'calendar-initial-window-hook #'realign-windows)
  (remove-hook 'org-present-mode-hook #'realign-windows)
  (remove-hook 'org-present-mode-quit-hook
               #'claude-ui//realign-org-present-quit-hook))

(defun claude-ui//realign-org-present-quit-hook (&optional frame)
  "This quit-hook will be run before quitting `org-present-mode'."
  (let ((org-present-mode nil))
    (realign-windows)))

(defun claude-ui/toggle-realign-padding ()
  "Toggle the realign padding effect in current buffer."
  (interactive)
  (setq-local claude--realign-p
              (not claude--realign-p))
  (realign-windows))

(defun claude-ui//theme-enabled-p (theme)
  "Return t if theme is currently loaded."
  (equal (spacemacs//get-theme-name theme) spacemacs--cur-theme))

(defun claude-ui/toggle-undecorated (&optional frame)
  "Toggle whether or not the selected frame is undecorated."
  (interactive)
  (set-frame-parameter
   frame 'undecorated
   (not (frame-parameter nil 'undecorated))))

(defvar-local claude--maximize-buffer-p nil
  "Whether the current buffer has been temporarily maximized.")

(defun claude-ui/toggle-maximize-buffer-on ()
  (interactive)
  (unless (one-window-p t)
    (setq-local claude--maximize-buffer-p t)
    (spacemacs/toggle-maximize-buffer)))

(defun claude-ui/toggle-maximize-buffer-off ()
  (interactive)
  (when (and (bound-and-true-p claude--maximize-buffer-p)
             (one-window-p t))
    (setq-local claude--maximize-buffer-p nil)
    (spacemacs/toggle-maximize-buffer)))
