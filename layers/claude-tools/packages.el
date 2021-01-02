;;; packages.el --- claude-tools layer packages file for Spacemacs.
;;
;; Copyright (c) 2019 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-tools-packages
  '(
    (beancount :location
               (recipe :fetcher github
                       :repo "beancount/beancount-mode"
                       :files ("beancount.el")))
    pdf-tools
    ))

(defun claude-tools/init-beancount ()
  (use-package beancount
    :mode ("\\.beancount\\'" . beancount-mode)
    :config
    (with-eval-after-load 'all-the-icons
      (add-to-list 'all-the-icons-mode-icon-alist
                   '(beancount-mode all-the-icons-octicon "file-text"
                                    :v-adjust 0.0
                                    :face all-the-icons-orange)))))

(defun claude-tools/post-init-pdf-tools ()
  (remove-hook 'pdf-view-mode-hook #'doom-modeline-set-pdf-modeline)
  (add-hook 'pdf-view-mode-hook #'doom-themes-hide-modeline)
  (with-eval-after-load 'pdf-tools
    (spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode
      "," 'pdf-view-fit-page-to-window)))
