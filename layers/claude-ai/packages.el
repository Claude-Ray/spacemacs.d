;;; packages.el --- claude-ai Layer packages file for Spacemacs
;;
;; Copyright (c) 2025 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-ai-packages
  '(
    copilot
    (tabnine :toggle claude-enable-tabnine)
    ))

(defun claude-ai/post-init-copilot ()
  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends))

  (with-eval-after-load 'copilot
    (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
    (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word))

  (add-hook 'prog-mode-hook 'copilot-mode))

(defun claude-ai/init-tabnine ()
  (use-package tabnine
    :config
    ;; disable inline previews
    (setq company-frontends '(company-pseudo-tooltip-frontend
                              company-echo-metadata-frontend))
    (define-key tabnine-completion-map (kbd "C-TAB") #'tabnine-accept-completion-by-word)
    (define-key tabnine-completion-map (kbd "C-<tab>") #'tabnine-accept-completion-by-word)
    (add-hook 'prog-mode-hook 'tabnine-mode)
    (add-hook 'kill-emacs-hook #'tabnine-kill-process)))
