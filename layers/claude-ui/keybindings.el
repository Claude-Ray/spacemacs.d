;;; keybindings.el --- claude-ui layer keybindings file for Spacemacs.
;;
;; Copyright (c) 2021 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs/set-leader-keys "wcf" #'claude-ui/toggle-realign-padding)

(spacemacs/set-leader-keys "Tp"
  'spacemacs/theme-transient-state/spacemacs/cycle-spacemacs-theme-backward)
