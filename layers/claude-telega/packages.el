;;; packages.el --- claude-telega layer packages file for Spacemacs.
;;
;; Copyright (c) 2019 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-telega-packages
  '(
    (telega :location
             (recipe :fetcher github
                     :repo "zevlg/telega.el"
                     :branch "master"
                     :files (:defaults "etc" "server" "Makefile")))
    ))

(defun claude-telega/init-telega()
  (use-package telega
    :commands (telega)
    :defer t
    :init
    (unless (display-graphic-p) (setq telega-use-images nil))
    :config
    (setq telega-proxies
          (list '(:server "127.0.0.1" :port 1082 :enable t
                     :type (:@type "proxyTypeSocks5"))))
    ))