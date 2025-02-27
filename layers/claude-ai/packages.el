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
    copilot-chat
    (tabnine :toggle claude-enable-tabnine)
    ))

(defun claude-ai/post-init-copilot ()
  (with-eval-after-load 'company
    ;; disable inline previews
    (setq company-frontends '(company-pseudo-tooltip-frontend
                              company-echo-metadata-frontend)))

  (with-eval-after-load 'copilot
    (setq copilot-indent-offset-warning-disable t
          copilot-max-char-warning-disable t)
    (add-to-list 'copilot-indentation-alist '(typescript-tsx-mode typescript-indent-level))
    (add-to-list 'copilot-indentation-alist '(sql-mode web-mode-sql-indent-offset))
    (add-to-list 'copilot-indentation-alist '(vue-mode web-mode-code-indent-offset))
    (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
    (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)
    (define-key copilot-completion-map (kbd "C-j") 'copilot-next-completion)
    (define-key copilot-completion-map (kbd "C-k") 'copilot-previous-completion))

  (add-hook 'prog-mode-hook 'copilot-mode))

(defun claude-ai/init-copilot-chat ()
  (use-package copilot-chat
    :defer t
    :config
    (setq copilot-chat-model "claude-3.7-sonnet"
          copilot-chat-prompt-suffix "Reply in Chinese")
    (add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message)))

(defun claude-ai/init-tabnine ()
  (use-package tabnine
    :config
    (define-key tabnine-completion-map (kbd "C-TAB") #'tabnine-accept-completion-by-word)
    (define-key tabnine-completion-map (kbd "C-<tab>") #'tabnine-accept-completion-by-word)
    (add-hook 'prog-mode-hook 'tabnine-mode)
    (add-hook 'kill-emacs-hook #'tabnine-kill-process)))
