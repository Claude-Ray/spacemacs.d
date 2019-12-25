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
    (let* ((badge-path spacemacs-badge-official-png)
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

;; Ligature
;; (when (eq window-system 'mac)
;;   (mac-auto-operator-composition-mode))

;; misc
(when (display-graphic-p)
  ;; Configuration for chinese font
  (spacemacs//set-monospaced-font "Source Code Pro" "Kaiti TC" 15 18)

  ;; Update treemacs theme with all-the-icons (via doom-themes)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)

  ;; Show dired icons
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
