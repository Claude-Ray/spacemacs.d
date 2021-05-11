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
    all-the-icons-dired
    all-the-icons-ibuffer
    all-the-icons-ivy-rich
    circadian
    diff-hl
    (dired :location built-in)
    diredfl
    doom-modeline
    doom-themes
    ranger
    (realign-mode :location
                  (recipe :fetcher github
                          :repo "amosbird/realign-mode.el"))
    writeroom-mode
    ))

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
                             ("18:00" . doom-gruvbox)))
    (advice-add 'circadian-enable-theme
                :before-until #'claude-ui//theme-enabled-p)
    (run-with-idle-timer 1 nil 'circadian-setup)))

(defun claude-ui/post-init-diff-hl ()
  ;; Display diff-hl between margins and buffer text
  (setq fringes-outside-margins nil)
  (setq diff-hl-draw-borders nil)
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
    ;; Update treemacs theme with all-the-icons.
    (when (display-graphic-p)
      (setq doom-themes-treemacs-theme "doom-colors")
      (doom-themes-treemacs-config))))

(defun claude-ui/post-init-ranger ()
  (setq ranger-footer-delay nil ; remove footer
        ranger-hidden-regexp '("^\\.\\|^node_modules$")
        ranger-override-dired 'ranger)
  (with-eval-after-load 'ranger
    (define-key ranger-mode-map (kbd "C-h") nil)))

(defun claude-ui/init-realign-mode ()
  (use-package realign-mode
    :defer t
    :commands realign-mode
    :init (run-with-idle-timer 1 nil 'realign-mode)
    :config
    ;; FIXME: Update width threshold according to the display
    (setq split-width-threshold 150)
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
