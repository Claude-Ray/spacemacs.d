;;; packages.el --- claude-ui Layer packages file for Spacemacs
;;
;; Copyright (c) 2019 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-ui-packages
  '(
    all-the-icons-dired
    all-the-icons-ibuffer
    all-the-icons-ivy-rich
    diff-hl
    diredfl
    doom-modeline
    doom-themes
    writeroom-mode
    ))

(defun claude-ui/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :config
    ;; Show dired icons.
    (when (display-graphic-p)
      (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))))

(defun claude-ui/init-all-the-icons-ibuffer ()
  (use-package all-the-icons-ibuffer
    :after ivy-rich
    :init (all-the-icons-ibuffer-mode 1)))

(defun claude-ui/init-all-the-icons-ivy-rich ()
  (use-package all-the-icons-ivy-rich
    :after ivy-rich
    :init (all-the-icons-ivy-rich-mode 1)
    :config
    (defun all-the-icons-ivy-rich--format-icon (icon)
      "Add support for ivy-avy by adding a space before the icon.
PATCHED: Add one more space because of the alignment issue."
      (format "  %s"
              (propertize
               icon
               'face `(:inherit ,(get-text-property 0 'face icon)
                                :height ,(if (numberp all-the-icons-ivy-rich-icon-size)
                                             all-the-icons-ivy-rich-icon-size
                                           1.0))
               'display '(raise -0.05))))
    ;; This hook is messing up the text alignment
    (remove-hook 'minibuffer-setup-hook #'all-the-icons-ivy-rich-align-icons)))

(defun claude-ui/post-init-diff-hl ()
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  ;; diff-hl-margin-mode works better with treemacs than diff-hl-mode,
  ;; and it's available in both terminal and GUI.
  (run-with-idle-timer 1 nil 'diff-hl-margin-mode))

(defun claude-ui/init-diredfl ()
  (use-package diredfl
    :config
    ;; Colorful dired.
    (diredfl-global-mode 1)))

(defun claude-ui/post-init-doom-modeline ()
  ;; Don’t compact font caches during GC.
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project))

(defun claude-ui/init-doom-themes ()
  (use-package doom-themes
    :defer t
    :config
    ;; Update treemacs theme with all-the-icons.
    (when (display-graphic-p)
      (setq doom-themes-treemacs-theme "doom-colors")
      (doom-themes-treemacs-config))))

(defun claude-ui/post-init-writeroom-mode ()
  (setq writeroom-width 120))
