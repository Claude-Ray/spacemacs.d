;;; funcs.el --- claude layer functions file for Spacemacs.
;;
;; Copyright (c) 2021 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun newline-and-indent (&optional arg)
  "Insert a newline, then indent according to major mode.
Indentation is done using the value of `indent-line-function'.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this command indents to the
column specified by the function `current-left-margin'.

With ARG, perform this action that many times.

FIXME:
Newline indentation between electric pairs is broken in Emacs 28,
since `newline-and-indent' and `reindent-then-newline-and-indent'
temporarily disable `electric-indent-mode' by this change
https://github.com/emacs-mirror/emacs/commit/b9d0cdcacbd3da93b4ebfa10d778efb618881ccc

This issue can be fixed by reverting the commit above.
REFERENCES:
https://github.com/emacs-ess/ESS/issues/1115"
  (interactive "*p")
  (delete-horizontal-space t)
  (unless arg
    (setq arg 1))
  (dotimes (_ arg)
    (newline nil t)
    (indent-according-to-mode)))

(defun claude//spacemacs-scratch-mode-hook ()
  "Disable confirmation prompt when killing the scratch buffer."
  (remove-hook 'kill-buffer-hook #'spacemacs//confirm-kill-buffer t))

(defun spacemacs/find-user-init-file ()
  "Edit the `user-init-file', in the current window.
FIXME: Compatibility fix for chemacs."
  (interactive)
  (find-file-existing (expand-file-name "init.el" user-emacs-directory)))

(defun unicode-fonts//setup-fonts (frame)
  "Setup `unicode-fonts' package for FRAME.

This functions setups `unicode-fonts' right away when starting a GUI Emacs.
But if Emacs is running in a daemon, it postpone the setup until a GUI frame
is opened.

FIXME: Symbol’s function definition is void: unicode-fonts//setup-fonts."
  (if (and frame (display-graphic-p frame))
      (with-selected-frame frame
        (require 'unicode-fonts)
        (unicode-fonts-setup)
        (remove-hook 'after-make-frame-functions #'unicode-fonts//setup-fonts))
    (add-hook 'after-make-frame-functions #'unicode-fonts//setup-fonts)))

(defun spacemacs/get-mode-line-theme-name ()
  "Return the mode-line theme name."
  (if (listp dotspacemacs-mode-line-theme)
      (car dotspacemacs-mode-line-theme)
    dotspacemacs-mode-line-theme))
