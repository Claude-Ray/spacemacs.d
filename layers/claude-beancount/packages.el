;;; packages.el --- claude-beancount Layer packages file for Spacemacs
;;
;; Copyright (c) 2019 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-beancount-packages
  '(
    (beancount :location local)
    ))

(defun claude-beancount/init-beancount ()
  (use-package beancount
    :mode ("\\.beancount\\'" . beancount-mode)
    :config
    (with-eval-after-load 'all-the-icons
      (add-to-list 'all-the-icons-mode-icon-alist
                   '(beancount-mode all-the-icons-octicon "file-text"
                                    :v-adjust 0.0
                                    :face all-the-icons-orange)))
    ))
