;;; keybindings.el --- claude-git layer keybindings file for Spacemacs.
;;
;; Copyright (c) 2021 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "h") #'magit-diff-toggle-refine-hunk)
  (define-key magit-mode-map (kbd ",i") #'claude-git/gitlab-init-feature)
  (define-key magit-mode-map (kbd ",m") #'claude-git/gitlab-merge-request)
  (define-key magit-mode-map (kbd ",r") #'claude-git/gitlab-review-dashboard))

(spacemacs/set-leader-keys (kbd "gh") 'vc-region-history)
