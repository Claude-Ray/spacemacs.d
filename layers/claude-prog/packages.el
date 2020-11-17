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
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(defun claude-prog/post-init-lsp-mode ()
  (setq lsp-auto-guess-root t
        ;; Disable lsp checker by default
        lsp-diagnostic-package :none
        ;; Disable watching files by default
        lsp-enable-file-watchers nil
        ;; Disable formatting on the fly
        lsp-enable-on-type-formatting nil
        ;; Disable lsp region formatting
        lsp-enable-indentation nil
        ;; Lean on flycheck-check-syntax-automatically
        lsp-flycheck-live-reporting nil
        ;; Auto kill lsp server
        lsp-keep-workspace-alive nil
        ;; Disable code actions on modeline
        lsp-modeline-code-actions-enable nil))

(defun claude-prog/post-init-js2-mode ()
  (setq-default js-indent-level 2
                js2-basic-offset 2)

  (setq js2-include-node-externs t
        ;; Let flycheck handle errors
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)

  (add-hook 'js2-mode-hook 'claude-prog//js2-mode-hook)
  (add-hook 'js2-mode-hook (lambda () (require 'dap-node))))
