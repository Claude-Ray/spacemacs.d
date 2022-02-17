;;; funcs.el --- claude-prog Layer functions File for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun claude-prog//js2-mode-hook ()
  (progn
    (define-key js2-mode-map "@" 'js-doc-insert-tag)
    (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
    (setq mode-name "JS2")
    (setq forward-sexp-function nil)
    (set (make-local-variable 'semantic-mode) nil)))

(defun claude-prog//typescript-mode-config (func mode)
  "Advice around `spacemacs/typescript-mode-config'.
Force the typescript-format keybinding to `,=='."
  (let ((typescript-backend 'lsp))
    (funcall func mode)))

(defun claude-prog//tsx-mode-hook ()
  "Append electric-pair for typescript-tsx-mode"
  (setq-local electric-pair-pairs `(,@electric-pair-pairs
                                    (?' . ?')
                                    (?` . ?`))))

(defun claude-prog//citre-peek-hook ()
  "Recenter the current window after `citre-peek'."
  (recenter)
  ;; Force reloading citre-peek-keymap
  (evil-normal-state))

(defun claude-prog//citre-set-jump-handler ()
  "Set jump handlers for citre."
  ;; FIXME: tide-jump-to-definition is incompatible with citre-jump
  (unless (bound-and-true-p tide-mode)
    (add-to-list 'spacemacs-jump-handlers 'citre-jump)))

(defun claude-prog//setup-lsp-jump-handler (func)
  "Advice around `spacemacs//setup-lsp-jump-handler'.
Set jump handler for LSP without async."
  (if citre-mode
      (add-to-list 'spacemacs-jump-handlers 'lsp-ui-peek-find-definitions)
    (funcall func)))

(defun claude-prog//kill-compilation-hook ()
  "Kill *complication* buffer and window automatically."
  (let* ((buffer (compilation-find-buffer))
         (window (get-buffer-window buffer)))
    (when window (delete-window window))
    (kill-buffer buffer)))

(defvar claude--contest-extname ".js"
  "File extension for a programming language.")

(defun claude-prog/contest ()
  "Create a file named by the prompt."
  (interactive)
  (let ((text (string-trim (read-from-minibuffer "title or URL:"))))
    (when (zerop (length text))
      (user-error "No URL"))
    (string-match "\\`.*?/?\\([a-zA-Z0-9-]*\\)\\(\\.\\w+\\)?/?\\'" text)
    (let ((title (match-string 1 text))
          (extname (match-string 2 text)))
      (when (zerop (length title))
        (user-error "No title matched"))
      (let ((file (expand-file-name
                   (concat title
                           (if (zerop (length extname))
                               claude--contest-extname
                             extname)))))
        (if (file-exists-p file)
            (message "Aborting, file already exists: %s" file)
          (let ((buffer (find-file-noselect file)))
            (with-current-buffer buffer
              (switch-to-buffer buffer))))))))

(defun claude-prog/quickrun (&rest plist)
  "Run `quickrun' for current buffer."
  (interactive)
  (save-buffer)
  (add-hook 'kill-buffer-hook #'quickrun--kill-quickrun-buffer nil t)
  (quickrun plist))

(defun claude-prog/quickrun-pop (&rest plist)
  "Run `quickrun' for current buffer with quickrun-focus-p."
  (interactive)
  (let ((quickrun-focus-p t))
    (claude-prog/quickrun plist)))

(defun claude-prog/quickrun-shell ()
  "Run `quickrun-shell' for current buffer with quickrun-focus-p."
  (interactive)
  (let ((quickrun-focus-p t))
    (save-buffer)
    (add-hook 'kill-buffer-hook #'quickrun--kill-quickrun-buffer nil t)
    (quickrun-shell)))

(defun claude-prog/smart-run ()
  "Compile the program including the current buffer."
  (interactive)
  (save-buffer)
  (cond ((or (eq major-mode 'js-mode)
             (eq major-mode 'js2-mode))
         (compile (concat "node " (file-relative-name buffer-file-name)))))
  (add-hook 'kill-buffer-hook #'claude-prog//kill-compilation-hook nil t))

(defun claude-prog/smart-run-pop ()
  "Compile the program including the current buffer then pop to *compilation*."
  (interactive)
  (claude-prog/smart-run)
  (pop-to-buffer "*compilation*"))

(defun claude-prog//sqlfmt-buffer-advice (func)
  "Advice around `sqlfmt-buffer', display the *sqlfmt* buffer when error occurs."
  (unless (ignore-errors
            (funcall func))
    (display-buffer "*sqlfmt*")))
