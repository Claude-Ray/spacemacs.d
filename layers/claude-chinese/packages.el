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
    (liberime :location
              (recipe :fetcher github
                      :repo "merrickluo/liberime"
                      :files ("CMakeLists.txt" "Makefile" "src" "liberime.el" "liberime-config.el"))
              :toggle (eq claude/chinese-default-input-method 'pyim))
    (pyim :requires posframe
          :toggle (eq claude/chinese-default-input-method 'pyim))
    (rime :toggle (eq claude/chinese-default-input-method 'rime))
    ))

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
    :config
    (defun rime--build-candidate-content ()
      "Custom candidate_format."
      (let* ((context (rime-lib-get-context))
             (candidates (alist-get 'candidates (alist-get 'menu context)))
             (menu (alist-get 'menu context))
             (highlighted-candidate-index (alist-get 'highlighted-candidate-index menu))
             (idx 1)
             (result ""))
        (when (and (rime--has-composition context) candidates)
          (dolist (c candidates)
            (let* ((curr (equal (1- idx) highlighted-candidate-index))
                   (candidates-text (concat
                                     (propertize
                                      (format "%d " idx)
                                      'face
                                      'rime-candidate-num-face)
                                     (if curr
                                         (propertize (car c) 'face 'rime-highlight-candidate-face)
                                       (propertize (car c) 'face 'rime-default-face))
                                     (if-let (comment (cdr c))
                                         (propertize (format " %s" comment) 'face 'rime-comment-face)
                                       ""))))
              (setq result (concat result
                                   candidates-text
                                   (rime--candidate-separator-char))))
            (setq idx (1+ idx))))
        result))
    (set-face-attribute 'rime-highlight-candidate-face nil
                        :foreground "White"
                        :background "DodgerBlue"
                        :bold nil)
    (when (spacemacs/system-is-mac)
      (setq rime-librime-root "~/.emacs.d/librime/dist"
            rime-user-data-dir (expand-file-name "~/Library/Rime/emacs/")))
    (when (spacemacs/system-is-linux)
      ;; F13 = XF86Tools
      (define-key global-map (kbd "<XF86Tools>") 'toggle-input-method)
      (setq rime-user-data-dir (expand-file-name "~/.config/fcitx/rime")))
    (setq default-input-method "rime"
          rime-show-preedit 'inline
          rime-posframe-style 'simple
          rime-posframe-properties
          (list :left-fringe 0
                :right-fringe 0)
          rime-show-candidate 'posframe)))
