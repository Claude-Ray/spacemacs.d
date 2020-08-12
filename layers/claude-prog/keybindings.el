;;; layers.el --- claude-prog Layer keybindings file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(with-eval-after-load 'js2-mode
  (define-key js-mode-map (kbd "s-r") 'claude-prog/quick-run))
