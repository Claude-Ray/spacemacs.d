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
    cc-mode
    ccls
    citre
    flycheck
    lsp-mode
    lua-mode
    js2-mode
    npm-mode
    ocamlformat
    ocp-indent
    python
    quickrun
    sqlfmt
    tide
    tuareg
    typescript-mode
    web-mode
    ))

(defun claude-prog/post-init-cc-mode ()
  (advice-add 'spacemacs//c-c++-setup-lsp-clangd
              :before #'claude-prog//c-c++-setup-lsp-advice))

(defun claude-prog/post-init-ccls ()
  (when (spacemacs/system-is-mac)
    ;; C++ headers
    (with-eval-after-load 'ccls
      (setq ccls-initialization-options
            `(:clang ,(list :extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                                        "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                                        "-isystem/usr/local/include"]
                            :resourceDir (string-trim (shell-command-to-string "clang -print-resource-dir"))))))))

(defun claude-prog/init-citre ()
  (use-package citre
    :defer t
    :init
    (require 'citre-config)
    :config
    (setq citre-auto-enable-citre-mode-modes '(prog-mode)
          citre-peek-file-content-height 20
          citre-project-root-function #'projectile-project-root)
    (setq-default citre-enable-xref-integration nil)
    (add-hook 'citre-mode-hook #'claude-prog//citre-set-jump-handler)
    (add-hook 'citre-peek--mode-hook #'claude-prog//citre-peek-hook)
    (advice-add 'spacemacs//setup-lsp-jump-handler
                :around #'claude-prog//setup-lsp-jump-handler)
    (advice-add 'citre-jump :around 'evil-better-jumper/set-jump-a)
    (advice-add 'citre-jump-back :around 'evil-better-jumper/set-jump-a)
    (advice-add 'citre-peek-jump :around 'evil-better-jumper/set-jump-a)))

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

        ;; Disable lsp checker, lean on flycheck-check-syntax-automatically
        lsp-diagnostics-provider :none
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

(defun claude-prog/post-init-lua-mode ()
  (when (spacemacs/system-is-linux)
    (with-eval-after-load 'lua-mode
      (setq lsp-clients-lua-language-server-bin
            (expand-file-name
             "lsp/lua-language-server/bin/Linux/lua-language-server"
             spacemacs-cache-directory)
            lsp-clients-lua-language-server-main-location
            (expand-file-name
             "lsp/lua-language-server/main.lua"
             spacemacs-cache-directory)))))

(defun claude-prog/post-init-js2-mode ()
  (setq-default js-indent-level 2
                js2-basic-offset 2)

  (setq js2-idle-timer-delay 0.5
        js2-include-node-externs t
        ;; Let flycheck handle errors
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)

  (add-hook 'js2-mode-hook #'claude-prog//js2-mode-hook))

(defun claude-prog/post-init-npm-mode ()
  (add-hook 'typescript-mode-hook #'npm-mode)
  (spacemacs/declare-prefix-for-mode 'typescript-mode "mn" "npm")
  (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
    "nc" 'npm-mode-npm-clean
    "ni" 'npm-mode-npm-install
    "nr" 'npm-mode-npm-run
    "ns" 'npm-mode-npm-install-save
    "nd" 'npm-mode-npm-install-save-dev
    "nn" 'npm-mode-npm-init
    "nu" 'npm-mode-npm-uninstall
    "nl" 'npm-mode-npm-list
    "np" 'npm-mode-visit-project-file)
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode
    "nc" 'npm-mode-npm-clean))

(defun claude-prog/init-ocamlformat ()
  (use-package ocamlformat
    :defer t
    :commands ocamlformat
    :init
    (add-hook 'tuareg-mode-hook #'claude-prog//ocaml-fmt-before-save-hook)
    :config
    (setq ocamlformat-enable 'enable-outside-detected-project)))

(defun claude-prog/init-ocp-indent ()
  (use-package ocp-indent
    :defer t
    :init
    (add-hook 'tuareg-mode-hook 'ocp-indent-caml-mode-setup)
    (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
      "==" 'ocp-indent-buffer)
    :config
    (defun ocp-indent-buffer ()
      "FIXME: Args out of range: 0 since emacs 28"
      (interactive nil)
      (ocp-indent-region 1 (buffer-size)))))

(defun claude-prog/post-init-python ()
  (setq python-indent-guess-indent-offset-verbose nil))

(defun claude-prog/init-quickrun ()
  (use-package quickrun
    :defer t
    :init
    (define-key prog-mode-map (kbd "s-r") #'claude-prog/quickrun)
    (define-key prog-mode-map (kbd "s-R") #'claude-prog/quickrun-pop)
    (define-key prog-mode-map (kbd "s-C-r") #'claude-prog/quickrun-shell)
    :config
    (setq quickrun-focus-p nil)
    (evil-define-key 'normal quickrun--mode-map (kbd "q") 'quit-window)
    (push '("*eshell-quickrun*"
            :dedicated t
            :position bottom
            :stick t
            :noselect t
            :height 0.3)
          popwin:special-display-config)
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

(defun claude-prog/post-init-tide ()
  (setq tide-format-options '(:tabSize 2 :indentSize 2)))

(defun claude-prog/post-init-tuareg ()
  (setq tuareg-opam-insinuate t)
  (with-eval-after-load 'tuareg
    (tuareg-opam-update-env (tuareg-opam-current-compiler))))

(defun claude-prog/post-init-typescript-mode ()
  (setq-default typescript-indent-level 2)
  (advice-add 'spacemacs/typescript-mode-config
              :around #'claude-prog//typescript-mode-config)
  (add-hook 'typescript-tsx-mode-hook #'claude-prog//tsx-mode-hook)
  (dolist (value '(tide typescript-formatter prettier))
    (add-to-list 'safe-local-variable-values
                 (cons 'typescript-fmt-tool value)))
  (dolist (value '(tslint eslint ))
    (add-to-list 'safe-local-variable-values
                 (cons 'typescript-linter value))))

(defun claude-prog/post-init-web-mode ()
  (setq-default
   css-indent-offset 2
   web-mode-attr-indent-offset 2
   web-mode-auto-close-style 2
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-markup-indent-offset 2))
