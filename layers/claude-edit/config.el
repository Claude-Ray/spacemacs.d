;;; config.el --- claude-edit layer configuration file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Replace smartparens
(electric-pair-mode t)
(setq electric-pair-inhibit-predicate #'claude-edit//electric-pair-inhibit)
(add-hook 'org-mode-hook #'claude-edit//electric-pair-inhibit-angle-brackets)

(remove-hook 'text-mode-hook #'auto-fill-mode)
