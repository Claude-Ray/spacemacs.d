;;; config.el --- claude-ui layer configuration file for Spacemacs.
;;
;; Copyright (c) 2019 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Always split two windows vertically
(setq split-height-threshold nil)
;; (setq split-width-threshold 160)

;; Modify the startup footer
(with-eval-after-load 'all-the-icons
  (setq dashboard-footer-icon
        (if (and (display-graphic-p)
                 (or (fboundp 'all-the-icons-fileicon)
                     (require 'all-the-icons nil 'noerror)))
            (all-the-icons-fileicon "emacs"
                                    :height 1.1
                                    :v-adjust -0.05
                                    :face 'font-lock-keyword-face)
          (propertize "Emacs" 'face 'dashboard-footer)))

  (defun spacemacs-buffer//insert-footer ()
    (let ((badge-path spacemacs-badge-official-png)
          (build-lhs "Happy hacking ")
          (build-rhs (if (display-graphic-p) " !" "!"))
          (buffer-read-only nil))
      (spacemacs-buffer/insert-page-break)
      (insert "\n")
      (insert build-lhs)
      (insert dashboard-footer-icon)
      (insert build-rhs)
      (spacemacs-buffer//center-line (+ (length build-lhs)
                                        (length dashboard-footer-icon)
                                        (length build-rhs)))
      (insert "\n"))))

;; Configuration for chinese font
;; (when (display-graphic-p)
;;   (spacemacs//set-monospaced-font "Source Code Pro" "Kaiti TC" 15 18))

;; Set larger font face for Info mode
(add-hook 'Info-mode-hook 'claude-ui//larger-buffer-face)

;; misc
(when (spacemacs/window-system-is-mac)
  ;; Maximize frames fully
  (setq frame-resize-pixelwise t)

  ;; Make undecorated frame resizable by dragging its internal borders
  (add-to-list 'default-frame-alist '(drag-internal-border . 1))
  (add-to-list 'default-frame-alist '(internal-border-width . 5))

  ;; Disable menu-bar in OSX GUI by default
  ;; (setq ns-auto-hide-menu-bar t)
  (menu-bar-mode -1))
