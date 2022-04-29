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
    confluence
    devdocs-browser
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

(defun claude-tools/post-init-confluence ()
  (setq confluence-save-credentials t)
  (with-eval-after-load 'confluence
    (require 'auth-source)
    (let* ((p (car (auth-source-search
                    :max 1 :user "confluence-url" :require '(:host))))
           (host (plist-get p :host))
           (url (if (functionp host) (funcall host) host)))
      (setq confluence-url url))))

(defun claude-tools/init-devdocs-browser ()
  (use-package devdocs-browser
    :defer t
    :init
    (setq devdocs-browser-cache-directory
          (expand-file-name "devdocs-browser" spacemacs-cache-directory))
    (global-set-key (kbd "C-c b") 'devdocs-browser-open)
    (global-set-key (kbd "C-c B") 'devdocs-browser-open-in)))

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
