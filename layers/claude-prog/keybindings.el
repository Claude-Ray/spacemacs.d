;;; keybindings.el --- claude-prog Layer keybindings file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(with-eval-after-load 'js2-mode
  (define-key js-mode-map (kbd "s-r") #'claude-prog/smart-run)
  (define-key js-mode-map (kbd "s-R") #'claude-prog/smart-run-pop)
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode
    "=e" #'claude-prog/web-format-buffer))

(with-eval-after-load 'web-mode
  (spacemacs/set-leader-keys-for-major-mode 'vue-mode
    "==" #'claude-prog/web-format-buffer))

(spacemacs/set-leader-keys ",c" #'claude-prog/contest)

(with-eval-after-load 'citre-mode
  (spacemacs/set-leader-keys-for-minor-mode 'citre-mode
    "gj" 'citre-jump
    "gJ" 'citre-jump-back
    "gp" 'citre-peek
    "gP" 'citre-ace-peek
    "gR" 'citre-peek-references
    "gu" 'citre-update-this-tags-file))

(setq citre-peek-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map [remap keyboard-quit] 'citre-peek-abort)
        map))

(evil-define-key 'normal citre-peek-keymap
  ;; Browse file
  (kbd "j") 'citre-peek-next-line
  (kbd "k") 'citre-peek-prev-line
  ;; Browse in the definition list
  (kbd "J") 'citre-peek-next-definition
  (kbd "K") 'citre-peek-prev-definition
  (kbd "C-n") 'citre-peek-next-definition
  (kbd "C-p") 'citre-peek-prev-definition
  ;; Browse in the history
  (kbd "h") 'citre-peek-chain-backward
  (kbd "l") 'citre-peek-chain-forward
  (kbd "H") 'citre-peek-prev-branch
  (kbd "L") 'citre-peek-next-branch
  ;; Modify history
  (kbd "f") 'citre-peek-through
  (kbd "M-d") 'citre-peek-delete-branch
  (kbd "M-D") 'citre-peek-delete-branches
  ;; Rearrange definition list
  (kbd "C-j") 'citre-peek-move-current-def-down
  (kbd "C-k") 'citre-peek-move-current-def-up
  (kbd "gm") 'citre-peek-make-current-def-first
  ;; Jump
  (kbd "RET") 'citre-peek-jump)
