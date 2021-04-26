;;; packages.el --- claude-prog Layer packages file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-prog-packages
  '(
    ccls
    flycheck
    lsp-mode
    js2-mode
    python
    quickrun
    sqlfmt
    typescript-mode
    ))

(defun claude-prog/post-init-ccls ()
  (when (spacemacs/system-is-mac)
    ;; C++ headers
    (with-eval-after-load 'ccls
      (setq ccls-initialization-options
            `(:clang ,(list :extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                                        "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                                        "-isystem/usr/local/include"]
                            :resourceDir (string-trim (shell-command-to-string "clang -print-resource-dir"))))))))

(defun claude-prog/post-init-flycheck ()
  ;; Allow flycheck to use eslint instead of eslint_d by .dir-locals.el
  (add-to-list 'safe-local-variable-values
               (cons 'flycheck-javascript-eslint-executable nil))
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(defun claude-prog/post-init-lsp-mode ()
  (setq lsp-auto-execute-action nil
        lsp-auto-guess-root t

        ;; Turn off for better performance
        lsp-enable-dap-auto-configure nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-imenu nil
        lsp-enable-indentation nil           ; no region formatting
        lsp-enable-links nil                 ; no clickable links
        lsp-enable-on-type-formatting nil    ; no formatting on the fly
        lsp-enable-snippet nil               ; handle yasnippet manually
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil

        ;; Disable lsp checker
        lsp-diagnostic-package :none
        ;; Lean on flycheck-check-syntax-automatically
        lsp-flycheck-live-reporting nil
        ;; Auto kill lsp server
        lsp-keep-workspace-alive nil

        ;; Prune minibuffer
        lsp-signature-doc-lines 5
        lsp-signature-render-documentation nil
        ;; Clean headerline
        lsp-headerline-breadcrumb-enable nil
        ;; Clean modeline
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil))

(defun claude-prog/post-init-js2-mode ()
  (setq-default js-indent-level 2
                js2-basic-offset 2)

  (setq js2-idle-timer-delay 0.5
        js2-include-node-externs t
        ;; Let flycheck handle errors
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)

  (add-hook 'js2-mode-hook #'claude-prog//js2-mode-hook))

(defun claude-prog/post-init-python ()
  (setq python-indent-guess-indent-offset-verbose nil))

(defun claude-prog/init-quickrun ()
  (use-package quickrun
    :defer t
    :init
    (define-key prog-mode-map (kbd "s-r") #'claude-prog/quickrun)
    (define-key prog-mode-map (kbd "s-R") #'claude-prog/quickrun-pop)
    :config
    (setq quickrun-focus-p nil)
    ;; Open *quickrun* in insert state
    (evil-set-initial-state 'quickrun--mode 'insert)
    (evil-define-key 'normal quickrun--mode-map (kbd "q") 'quit-window)
    (push '("*quickrun*"
            :dedicated t
            :position bottom
            :stick t
            :noselect t
            :height 0.3)
          popwin:special-display-config)))

(defun claude-prog/post-init-sqlfmt ()
  (push '("*sqlfmt*"
          :dedicated t
          :position bottom
          :stick t
          :noselect t
          :height 0.3)
        popwin:special-display-config)
  (advice-add 'sqlfmt-buffer :around #'claude-prog//sqlfmt-buffer-advice))

(defun claude-prog/post-init-typescript-mode ()
  (setq-default typescript-indent-level 2))
