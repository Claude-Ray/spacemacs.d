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
    diff-hl
    diredfl
    doom-modeline
    doom-themes
    ))

(defun claude-ui/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :config
    ;; Show dired icons.
    (when (display-graphic-p)
      (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))))

(defun claude-ui/post-init-diff-hl ()
  (use-package diff-hl
    :defer t
    :config
    ;; diff-hl-margin-mode works better with treemacs than diff-hl-mode,
    ;; and it's available in both terminal and GUI.
    (run-with-idle-timer 1 nil 'diff-hl-margin-mode)))

(defun claude-ui/init-diredfl ()
  (use-package diredfl
    :config
    ;; Colorful dired.
    (diredfl-global-mode 1)))

(defun claude-ui/post-init-doom-modeline ()
  (use-package doom-modeline
    :defer t
    :config
    ;; Don’t compact font caches during GC.
    (setq inhibit-compacting-font-caches t)
    (setq doom-modeline-buffer-file-name-style 'relative-from-project)))

(defun claude-ui/init-doom-themes ()
  (use-package doom-themes
    :defer t
    :config
    ;; Update treemacs theme with all-the-icons.
    (when (display-graphic-p)
      (setq doom-themes-treemacs-theme "doom-colors")
      (doom-themes-treemacs-config))))
