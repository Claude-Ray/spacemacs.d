;;; packages.el --- claude-chinese Layer packages file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-chinese-packages
  '(
    go-translate
    (liberime :location
              (recipe :fetcher github
                      :repo "merrickluo/liberime"
                      :files ("CMakeLists.txt" "Makefile" "src" "liberime.el" "liberime-config.el"))
              :toggle (eq claude/chinese-default-input-method 'pyim))
    pangu-spacing
    (pyim :requires posframe
          :toggle (eq claude/chinese-default-input-method 'pyim))
    (rime :toggle (eq claude/chinese-default-input-method 'rime))
    ))

(defun claude-chinese/init-go-translate ()
  (use-package go-translate
    :defer t
    :init
    (spacemacs/set-leader-keys "ot" #'claude-chinese/gt-to-translate)
    (spacemacs/set-leader-keys "oT" #'claude-chinese/gt-prompt-translate)
    :config
    (setq gt-buffer-render-follow-p t
          gt-default-translator
          (gt-translator
           :taker (gt-taker)
           :engines (gt-google-engine)
           :render (gt-buffer-render))
          gt-langs '("en" "zh"))))

(defun claude-chinese/post-init-pangu-spacing ()
  (global-pangu-spacing-mode -1))

(defun claude-chinese/init-liberime ()
  (use-package liberime
    :init
    (setq liberime-auto-build t)
    (setq liberime-user-data-dir (expand-file-name "~/Library/Rime/emacs/"))
    (setq default-input-method "pyim")
    (setq pyim-titles '("ㄓ " "PYIM-EN " "PYIM-AU "))
    (add-hook 'after-init-hook #'liberime-sync)))

(defun claude-chinese/init-pyim ()
  (use-package pyim
    :defer t
    :after liberime
    :config
    (setq pyim-default-scheme 'rime
          pyim-directory (expand-file-name "pyim/" spacemacs-cache-directory)
          pyim-dcache-directory (expand-file-name "dcache/" pyim-directory)
          pyim-page-length 9)
    (evilified-state-evilify pyim-dm-mode pyim-dm-mode-map)
    (when (display-graphic-p)
      (setq pyim-page-tooltip 'posframe))
    (setq pyim-punctuation-dict
          '(("'" "‘" "’")
            ("\"" "“" "”")
            ("_" "——")
            ("^" "……")
            ("]" "】")
            ("[" "【")
            ("?" "？")
            (">" "》")
            ("<" "《")
            (";" "；")
            (":" "：")
            ("\\" "、")
            ("." "。")
            ("," "，")
            ("*" "×")
            (")" "）")
            ("(" "（")
            ("$" "￥")
            ("!" "！")
            ("~" "～")
            ("}" "｝")
            ("{" "｛")))
    (let ((map pyim-mode-map))
      (define-key map (kbd "C-n") 'pyim-page-next-page)
      (define-key map (kbd "C-p") 'pyim-page-previous-page))))

(defun claude-chinese/init-rime ()
  (use-package rime
    :defer t
    :init
    (setq default-input-method "rime")
    (when (spacemacs/system-is-mac)
      (when (file-exists-p "/opt/homebrew/opt/emacs-plus")
        (setq rime-emacs-module-header-root
              "/opt/homebrew/opt/emacs-plus/include"))
      (setq rime-librime-root "~/.emacs.d/librime/dist"
            rime-user-data-dir (expand-file-name "~/Library/Rime/emacs/")))
    (when (spacemacs/system-is-linux)
      ;; F13 = XF86Tools = 269025153(keysym)
      (define-key global-map (kbd "<XF86Tools>") 'toggle-input-method)
      (define-key global-map (kbd "<269025153>") 'toggle-input-method)

      (setq rime-user-data-dir
            (expand-file-name (if (executable-find "fcitx5")
                                  "~/.local/share/fcitx5/rime/emacs"
                                "~/.config/fcitx/rime/emacs"))))
    :config
    (set-face-attribute 'rime-highlight-candidate-face nil
                        :foreground "White"
                        :background "DodgerBlue"
                        :bold nil)
    (setq rime-candidate-num-format-function
          #'claude-chinese//rime-candidate-num-format
          rime-show-preedit 'inline
          rime-posframe-style 'simple
          rime-posframe-properties
          (list :left-fringe 0
                :right-fringe 0)
          rime-show-candidate
          ;; Both posframe and popup have performance issues in native-comp
          (if (version<= "28" emacs-version) 'minibuffer 'posframe))))
