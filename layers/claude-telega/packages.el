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
                    :files (:defaults "etc" "server" "Makefile")))))

(defun claude-telega/init-telega ()
  (use-package telega
    :commands (telega)
    :defer t
    :init
    (unless (display-graphic-p) (setq telega-use-images nil))
    :config
    (setq telega-proxies
          (list '(:server "127.0.0.1" :port 1082 :enable t
                          :type (:@type "proxyTypeSocks5"))))
    (with-eval-after-load 'all-the-icons
      (add-to-list 'all-the-icons-mode-icon-alist
                   '(telega-root-mode all-the-icons-fileicon "telegram"
                                      :heigt 1.0
                                      :v-adjust -0.2
                                      :face all-the-icons-yellow))
      (add-to-list 'all-the-icons-mode-icon-alist
                   '(telega-chat-mode all-the-icons-fileicon "telegram"
                                      :heigt 1.0
                                      :v-adjust -0.2
                                      :face all-the-icons-blue)))
    (define-key telega-msg-button-map (kbd "k") nil)
    (define-key telega-msg-button-map (kbd "l") nil)))
