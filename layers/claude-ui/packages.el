;;; packages.el --- claude-ui Layer packages file for Spacemacs
;;
;; Copyright (c) 2019 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-ui-packages
  '(
    all-the-icons
    all-the-icons-dired
    all-the-icons-ibuffer
    all-the-icons-ivy-rich
    circadian
    diff-hl
    (dired :location built-in)
    diredfl
    doom-modeline
    doom-themes
    (pragmatapro :location local)
    ranger
    (realign-mode :location
                  (recipe :fetcher github
                          :repo "amosbird/realign-mode.el"))
    writeroom-mode
    zenburn-theme
    ))

(defun claude-ui/post-init-all-the-icons ()
  (with-eval-after-load 'all-the-icons
    (dolist (icon
             '(("bat"  all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver)
               ("cmd"  all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver)
               ("conf" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow)
               ("epub" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
               ("exe"  all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-dsilver)
               ("http" all-the-icons-material "http" :face all-the-icons-blue)
               ("mjs" all-the-icons-alltheicon "javascript" :height 1.0 :v-adjust 0.0 :face all-the-icons-yellow)
               ("mobi" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
               ("plantuml" all-the-icons-material "border_color" :face all-the-icons-green)
               ("toml" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow)))
      (add-to-list 'all-the-icons-extension-icon-alist icon))
    (add-to-list 'all-the-icons-regexp-icon-alist
                 '("persp-auto-save" all-the-icons-fileicon "elisp" :v-adjust -0.2 :face all-the-icons-dsilver))))

(defun claude-ui/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :if (display-graphic-p)
    :after dired
    :config
    (setq all-the-icons-dired-monochrome nil)
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

(defun claude-ui/init-all-the-icons-ibuffer ()
  (use-package all-the-icons-ibuffer
    :after ivy-rich
    :init (all-the-icons-ibuffer-mode 1)))

(defun claude-ui/init-all-the-icons-ivy-rich ()
  (use-package all-the-icons-ivy-rich
    :after ivy-rich
    :init (all-the-icons-ivy-rich-mode 1)))

(defun claude-ui/init-circadian ()
  (use-package circadian
    :if (display-graphic-p)
    :init
    (setq circadian-themes '(("8:00" . doom-solarized-light)
                             ("11:25" . spacemacs-light)
                             ("13:25" . doom-solarized-light)
                             ("18:00" . zenburn)))
    (advice-add 'circadian-enable-theme
                :before-until #'claude-ui//theme-enabled-p)
    (run-with-idle-timer 1 nil 'circadian-setup)))

(defun claude-ui/post-init-diff-hl ()
  ;; Display diff-hl between margins and buffer text
  (setq fringes-outside-margins nil)
  (setq diff-hl-draw-borders nil)
  ;; Display diff-hl on the margin in GUI mode.
  (unless (display-graphic-p)
    (run-with-idle-timer 1 nil 'diff-hl-margin-mode))
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote))

(defun claude-ui/post-init-dired ()
  (when (spacemacs/system-is-mac)
    ;; Use gls instead of ls
    (setq dired-use-ls-dired t
          insert-directory-program "/usr/local/bin/gls"
          dired-listing-switches "-aBhl --group-directories-first")))

(defun claude-ui/init-diredfl ()
  (use-package diredfl
    :after dired
    :config
    ;; Colorful dired.
    (diredfl-global-mode 1)))

(defun claude-ui/init-doom-modeline ()
  ;; doom modeline depends on `display-graphic-p' so we delay its initialization
  ;; as when dumping we don't know yet wether we are using a graphical emacs or
  ;; not.
  (spacemacs|unless-dumping-and-eval-after-loaded-dump doom-modeline
    (use-package doom-modeline
      :defer t
      :init (doom-modeline-mode)
      :config
      (add-to-list 'all-the-icons-mode-icon-alist
                   '(calendar-mode all-the-icons-faicon "calendar"
                                   :v-adjust -0.1
                                   :face all-the-icons-red))
      (add-to-list 'all-the-icons-mode-icon-alist
                   '(spacemacs-buffer-mode all-the-icons-faicon "home"
                                           :v-adjust -0.1
                                           :face font-lock-keyword-face))
      ;; Don’t compact font caches during GC.
      (setq inhibit-compacting-font-caches t)
      (setq doom-modeline-mu4e t
            doom-modeline-buffer-state-icon nil
            doom-modeline-buffer-file-name-style 'relative-from-project))))

(defun claude-ui/init-doom-themes ()
  (use-package doom-themes
    :defer t
    :config
    (setq doom-gruvbox-dark-variant "soft")
    (custom-set-faces
     `(markdown-bold-face ((t (:foreground ,(doom-color 'fg)))))
     '(markdown-header-face-1 ((t (:inherit outline-1))))
     '(markdown-header-face-2 ((t (:inherit outline-2))))
     '(markdown-header-face-3 ((t (:inherit outline-3))))
     '(markdown-header-face-4 ((t (:inherit outline-4))))
     '(markdown-header-face-5 ((t (:inherit outline-5))))
     '(markdown-header-face-6 ((t (:inherit outline-6))))
     '(markdown-header-face-7 ((t (:inherit outline-7))))
     '(markdown-header-face-8 ((t (:inherit outline-8)))))
    ;; Update treemacs theme with all-the-icons.
    (when (display-graphic-p)
      (setq doom-themes-treemacs-theme "doom-colors")
      (doom-themes-treemacs-config))))

(defun claude-ui/init-pragmatapro ()
  (when (display-graphic-p)
    (require 'pragmatapro-prettify-symbols)
    (add-hook 'prog-mode-hook 'pragmatapro-prettify-symbols-hook)))

(defun claude-ui/post-init-ranger ()
  (setq ranger-footer-delay nil ; remove footer
        ranger-hidden-regexp '("^\\.\\|^node_modules$")
        ranger-override-dired 'ranger
        ranger-show-literal nil)
  (when ranger-override-dired
    (spacemacs/set-leader-keys "fj" ranger-override-dired))
  (with-eval-after-load 'ranger
    (add-hook 'magit-mode-hook #'claude-ui/ranger-minimal)
    (define-key ranger-mode-map (kbd "C-h") nil)))

(defun claude-ui/init-realign-mode ()
  (use-package realign-mode
    :defer t
    :commands realign-mode
    :init (run-with-idle-timer 1 nil 'realign-mode)
    :config
    ;; Resize width threshold to the golden ratio.
    ;; FIXME: It only depends on the current screen.
    (setq split-width-threshold (truncate (* (frame-width) .618)))
    (advice-add 'realign-turn-on :after #'claude-ui//realign-turn-on)
    (advice-add 'realign-turn-off :after #'claude-ui//realign-turn-off)
    (push #'claude-ui//realign-need-padding-p
          realign-need-padding-predicates)
    (push #'claude-ui//realign-ignore-window-p
          realign-ignore-window-predicates)))

(defun claude-ui/post-init-writeroom-mode ()
  (setq writeroom-fringes-outside-margins nil
        writeroom-fullscreen-effect 'maximized
        writeroom-width 120))

(defun claude-ui/init-zenburn-theme ()
  (use-package zenburn-theme
    :defer t))
