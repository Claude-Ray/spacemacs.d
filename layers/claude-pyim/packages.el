;;; packages.el --- claude-pyim Layer packages file for Spacemacs
;;
;; Copyright (c) 2019 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-pyim-packages
  '(
    (liberime :location
              (recipe :fetcher github
                      :repo "merrickluo/liberime"
                      :files ("CMakeLists.txt" "Makefile" "src" "liberime.el" "liberime-config.el")))
    (pyim :requires posframe)
    ))

(defun claude-pyim/init-liberime ()
  (use-package liberime
    :init
    (setq liberime-user-data-dir (expand-file-name "~/Library/Rime/emacs/"))
    (setq default-input-method "pyim")
    (setq pyim-titles '("ㄓ " "PYIM-EN " "PYIM-AU "))
    (add-hook 'after-init-hook #'liberime-sync)
    :config
    (unless (file-exists-p (concat (liberime-get-library-directory)
                                   "src/liberime-core"
                                   module-file-suffix))
      (liberime-build))))

(defun claude-pyim/init-pyim ()
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
