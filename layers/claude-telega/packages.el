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
    popwin
    (telega :location
            (recipe :fetcher github
                    :repo "zevlg/telega.el"
                    :branch "master"
                    :files (:defaults "etc" "server" "Makefile")))))

(defun claude-telega/post-init-popwin ()
  (push '("*Telega Root*"
          :dedicated t
          :position right
          :stick t
          :noselect t
          :width 60)
        popwin:special-display-config)
  (push '("^◀[[({<].*[\])>}]$"
          :regexp t
          :dedicated t
          :position right
          :stick t
          :noselect t
          :width 60)
        popwin:special-display-config))

(defun claude-telega/init-telega ()
  (use-package telega
    :commands (telega)
    :defer t
    :init
    (unless (display-graphic-p) (setq telega-use-images nil))
    :config
    ;; NOTE: Fix mode line by resetting width
    (doom-modeline--set-char-widths doom-modeline-rhs-icons-alist)
    (setq telega-proxies
          (list '(:server "127.0.0.1" :port 7891 :enable t
                          :type (:@type "proxyTypeSocks5"))))
    (setq telega-chat-button-width 28
          telega-chat-fill-column 47
          telega-root-fill-column 48)
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
