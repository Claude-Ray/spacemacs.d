;;; funcs.el --- claude-prog Layer functions File for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude-prog//js2-mode-hook ()
  (progn
    (define-key js2-mode-map "@" 'js-doc-insert-tag)
    (setq mode-name "JS2")
    (setq forward-sexp-function nil)
    (set (make-local-variable 'semantic-mode) nil)))
