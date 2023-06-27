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
    cal-china-x
    (calfw :toggle claude-enable-calfw)
    (calfw-org :toggle claude-enable-calfw)
    (confluence :location local)
    devdocs-browser
    eww
    leetcode
    pdf-tools
    restclient
    sicp
    tree-sitter
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

(defun claude-tools/init-cal-china-x ()
  (use-package cal-china-x
    :after calendar
    :init
    (setq calendar-mark-holidays-flag t)
    :config
    (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
    (setq calendar-holidays
          (append cal-china-x-important-holidays
                  cal-china-x-general-holidays))))

(defun claude-tools/init-calfw ()
  (use-package calfw
    :defer t
    :config
    (evil-set-initial-state 'cfw:calendar-mode 'normal)
    (add-hook 'cfw:calendar-mode-hook #'claude-ui/toggle-realign-padding)))

(defun claude-tools/init-calfw-org ()
  (use-package calfw-org
    :defer t
    :commands cfw:open-org-calendar
    :config
    (let ((map cfw:org-schedule-map))
      (define-key map (kbd "TAB") 'cfw:org-open-agenda-day)
      (evil-define-key 'normal map (kbd "q") 'bury-buffer))))

(defun claude-tools/init-confluence ()
  (use-package confluence
    :defer t
    :commands (confluence-get-page confluence-search)
    :config
    (progn
      ;; remove the hook on buffer save that automatically store the buffer
      ;; in confluence, it creates a lot of useless revision in a page history.
      (advice-add 'confluence-base-mode-init
                  :after 'spacemacs//confluence-remove-save-hook)
      (dolist (mode '(confluence-mode
                      confluence-xml-mode
                      confluence-search-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "s" 'spacemacs/confluence-save-to-confluence-minor-edit)
        (spacemacs/set-leader-keys-for-major-mode mode
          "S" 'spacemacs/confluence-save-to-confluence-major-edit)
        (spacemacs/set-leader-keys-for-major-mode mode
          "TAB" 'confluence-toggle-page-content-type))
      (setq confluence-save-credentials t)
      (require 'auth-source)
      (let* ((p (car (auth-source-search
                      :max 1 :user "confluence-url" :require '(:host))))
             (host (plist-get p :host))
             (url (if (functionp host) (funcall host) host)))
        (setq confluence-url url)))))

(defun claude-tools/init-devdocs-browser ()
  (use-package devdocs-browser
    :defer t
    :init
    (setq devdocs-browser-cache-directory
          (expand-file-name "devdocs-browser" spacemacs-cache-directory))
    (spacemacs/set-leader-keys "ob" #'devdocs-browser-open)
    (spacemacs/set-leader-keys "oB" #'devdocs-browser-open-in)))

(defun claude-tools/init-eww ()
  (use-package eww
    :defer t
    :config
    (let ((mode 'eww-mode))
      (spacemacs/declare-prefix-for-mode mode "mv" "view")
      (spacemacs/declare-prefix-for-mode mode "ml" "list")
      (spacemacs/set-leader-keys-for-major-mode mode
        "o" 'browse-web
        "r" 'eww-reload
        "h" 'eww-list-histories
        "a" 'eww-add-bookmark
        "lb" 'eww-list-buffers
        "lo" 'eww-list-bookmarks
        "vx" 'eww-browse-with-external-browser
        "vf" 'eww-toggle-fonts
        "vr" 'eww-readable
        "vs" 'eww-view-source)
      (evil-define-key 'normal eww-mode-map
        "J" 'eww-back-url
        "K" 'eww-forward-url
        "H" 'eww-back-url
        "L" 'eww-forward-url
        "[" 'eww-previous-url
        "]" 'eww-next-url
        (kbd "C-j") 'shr-next-link
        (kbd "C-k") 'shr-previous-link))))

(defun claude-tools/init-leetcode ()
  (use-package leetcode
    :defer t
    :config
    (setq leetcode-prefer-language "javascript"
          leetcode-prefer-sql "mysql"
          leetcode-save-solutions t
          leetcode-directory "~/Documents/leetcode/Algorithms")
    (define-key leetcode--problems-mode-map
      (kbd "<return>") 'leetcode-show-current-problem)))

(defun claude-tools/post-init-pdf-tools ()
  (remove-hook 'pdf-view-mode-hook #'doom-modeline-set-pdf-modeline)
  (add-hook 'pdf-view-mode-hook #'doom-themes-hide-modeline)
  (with-eval-after-load 'pdf-tools
    (spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode
      "," 'pdf-view-fit-page-to-window)))

(defun claude-tools/post-init-restclient ()
  ;; Avoid `tree-sitter' throwing an error when parsing
  ;; decoded-http-response-buffer until `restclient-prettify-response' is done.
  (when tree-sitter-syntax-highlight-enable
    (advice-add 'restclient-decode-response
                :before #'claude-tools//restclient-clear-response)))

(defun claude-tools/init-sicp ()
  (use-package sicp
    :defer t
    :init
    (defun info-sicp ()
      "Display the SICP in Info mode."
      (interactive)
      (info "sicp"))))

(defun claude-tools/post-init-tree-sitter ()
  (with-eval-after-load 'tree-sitter
    (when (file-exists-p (concat (tree-sitter-cli-bin-directory) "vue.so"))
      (tree-sitter-load 'vue "vue")
      (add-to-list 'tree-sitter-major-mode-language-alist '(vue-mode . vue))
      (add-to-list 'tree-sitter-major-mode-language-alist '(web-mode . vue))))
  ;; XXX: Turn on tree-sitter without deferring
  (when tree-sitter-syntax-highlight-enable
    (custom-set-faces
     '(font-lock-constant-face ((t (:weight normal))))
     '(tree-sitter-hl-face:function.call ((t (:weight normal)))))
    (run-with-idle-timer 1 nil 'global-tree-sitter-mode)))
