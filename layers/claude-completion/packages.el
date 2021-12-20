;;; packages.el --- claude-completion layer packages file for Spacemacs.
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-completion-packages
  '(
    auto-complete
    company
    smartparens
    yasnippet
    ))

(defun claude-completion/post-init-auto-complete ()
  ;; Avoid annoying complete
  (setq tab-always-indent t))

(defun claude-completion/post-init-company ()
  (advice-add 'company-mode :around #'claude-completion//company-advice)
  (with-eval-after-load 'company
    (let ((map company-active-map))
      (define-key map (kbd "C-n")
        'company-select-next-if-tooltip-visible-or-complete-selection)
      (define-key map (kbd "C-p") 'company-select-previous)
      (define-key map (kbd "C-d") 'company-next-page)
      (define-key map (kbd "C-u") 'company-previous-page)
      (define-key map (kbd "C-h") 'company-show-doc-buffer))
    (setq company-format-margin-function nil
          company-show-numbers t
          company-transformers nil)))

(defun claude-completion/post-init-smartparens ()
  (spacemacs/defer-until-after-user-config
   #'claude-completion//smartparens-advice))

(defun claude-completion/post-init-yasnippet ()
  (define-key evil-insert-state-map (kbd "M-TAB") 'yas-expand)
  (define-key evil-insert-state-map (kbd "M-i") 'ivy-yasnippet)
  (with-eval-after-load 'yasnippet
    (evil-define-key 'insert yas-minor-mode-map (kbd "TAB") yas-maybe-expand)))
