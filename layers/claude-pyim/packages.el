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
    (liberime :location local)
    posframe
    pyim
    ))

(defun claude-pyim/init-liberime ()
  (use-package liberime
    :after pyim
    :config
    (liberime-start "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"
                    (file-truename "~/Library/Rime/emacs"))
    (liberime-select-schema "double_pinyin_flypy")
    (setq pyim-default-scheme 'rime)))

(defun claude-pyim/init-posframe ()
  (use-package posframe
    :defer t))

(defun claude-pyim/post-init-pyim()
  (use-package pyim
    :config
    (setq default-input-method "pyim")
    (when (display-graphic-p)
      (setq pyim-page-tooltip 'posframe))
    (let ((map pyim-mode-map))
      (define-key map (kbd "C-n") 'pyim-page-next-page)
      (define-key map (kbd "C-p") 'pyim-page-previous-page))
    (setq pyim-page-length 9)))
