;;; config.el --- claude-prog Layer configuration file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|define-jump-handlers json-mode)

(setq-default sh-basic-offset 2)

(defvar claude-enable-codeium nil
  "If non nil then codeium is enabled.")
