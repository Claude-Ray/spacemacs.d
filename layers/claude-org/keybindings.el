;;; layers.el --- claude-org Layer keybindings file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
  "SPC" #'spacemacs/org-agenda-transient-state/body
  "sb" #'claude-org/org-agenda-tree-to-indirect-buffer)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "==" #'claude-org/org-format
  "Cs" #'claude-org/org-clock-schedule)
