;;; packages.el --- claude-org Layer packages file for Spacemacs
;;
;; Copyright (c) 2020 claude-ray
;;
;; Author: Claude <yunleiqi@gmail.com>
;; URL: https://github.com/Claude-Ray/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst claude-org-packages
  '(
    (org :location built-in)
    org-present
    org-roam
    org-roam-server
    plantuml-mode
    valign
    ))

(defun claude-org/post-init-org ()
  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-protocol t))

  (setq org-directory "~/Documents/Org"
        org-default-notes-file (expand-file-name "inbox.org" org-directory))

  (setq org-adapt-indentation nil ; no leading spaces
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0
        org-src-preserve-indentation t ; no leading spaces before src blocks
        org-src-window-setup 'current-window
        org-startup-folded t
        org-startup-indented t
        org-startup-truncated nil)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n!)" "WIP(i!)" "WAIT(w@/!)" "|")
          (sequence "|" "DONE(d)" "CANCELED(c@)")))
  (setq org-todo-keyword-faces
        '(("WIP" . (:foreground "DodgerBlue" :weight bold))
          ("NEXT" :foreground "SeaGreen" :weight bold)
          ("WAIT" :foreground "DarkGoldenrod" :weight bold)
          ("CANCELED" . (:foreground "DarkGray" :weight bold))))

  ;; org-agenda
  (setq org-agenda-files (list org-directory)
        org-agenda-log-mode-items '(closed clock state)
        org-agenda-span 'day
        org-agenda-start-with-log-mode t)

  (setq claude--org-inbox-file org-default-notes-file
        claude--org-journal-file (expand-file-name "journal.org" org-directory)
        claude--org-note-file (expand-file-name "note.org" org-directory)
        claude--org-todo-file (expand-file-name "todo.org" org-directory)
        claude--org-work-file (expand-file-name "work.org" org-directory))

  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODOs"
           ((agenda "")
            (alltodo "")))
          (" " "Agenda"
           ((agenda ""
                    ((org-agenda-clockreport-mode t)))))
          ("f" . "Work Review")
          ("ff" "Work Agenda"
           ((agenda ""
                    ((org-agenda-tag-filter-preset '("+WORK"))
                     (org-agenda-clockreport-mode t)
                     (org-agenda-files (list claude--org-work-file))))))
          ("ft" "Work TODOs"
           ((tags-todo "WORK"
                       ((org-agenda-overriding-header "Work Tasks")
                        (org-agenda-files (list claude--org-work-file))))))
          ("fd" "Work Agenda and TODOs"
           ((agenda "")
            (tags-todo "WORK"))
           ((org-agenda-tag-filter-preset '("+WORK"))
            (org-agenda-start-with-clockreport-mode t)
            (org-agenda-files (list claude--org-work-file))))
          ("j" . "Personal Review")
          ("jj" "Personal Agenda"
           ((agenda ""
                    ((org-agenda-tag-filter-preset '("-WORK"))))))
          ("jk" "Personal Agenda and TODOs"
           ((agenda "")
            (tags-todo "-WORK"))
           ((org-agenda-tag-filter-preset '("-WORK"))))))

  ;; org-capture
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline claude--org-inbox-file "Tasks")
           "* TODO %?\n %i\n %a")
          ("w" "Work" entry (file+headline claude--org-inbox-file "Works")
           "* TODO %?\n %i\n %U")
          ("i" "Idea" entry (file+headline claude--org-inbox-file "Ideas")
           "* TODO %?\n %i\n %U")
          ("m" "Mark" entry (file+headline claude--org-inbox-file "Marks")
           "* %?\n %i\n %U %a")
          ("n" "Note" entry (file+headline claude--org-inbox-file "Notes")
           "* %?\n %i\n %U")
          ("j" "Journal" entry (file+headline claude--org-journal-file "Journals")
           "* %?\n %i\n %U"
           :empty-lines 1)))

  ;; org-refile
  (setq org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-outline-path 'file
        org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 2))
        org-refile-target-verify-function #'claude-org//verify-refile-target)
  ;; Update statistics cookies where the item was refiled to.
  (add-hook 'org-after-refile-insert-hook 'org-update-parent-todo-statistics)
  ;; Update statistics cookies where the item was refiled from.
  (advice-add 'org-refile :around #'claude-org//org-refile-advice)

  ;; org-archive
  (setq org-archive-location "%s_archive::* Archived Tasks")

  (advice-add 'org-cycle :around #'claude-org//org-cycle-advice))

(defun claude-org/post-init-org-present ()
  (with-eval-after-load 'org-present
    (defun org-present ()
      "Init `org-present-narrow' after `org-present-mode-hook'.
Make sure images are shown inline in all slides.

Waiting for pending PR https://github.com/rlister/org-present/pull/31"
      (interactive)
      (setq org-present-mode t)
      (org-present-add-overlays)
      (run-hooks 'org-present-mode-hook)
      (org-present-narrow)
      (org-present-run-after-navigate-functions))))

(defun claude-org/init-org-roam ()
  (use-package org-roam-protocol
    :after org-protocol)

  (use-package org-roam
    :defer t
    :commands (org-roam-buffer-toggle-display
               org-roam-dailies-find-yesterday
               org-roam-dailies-find-today
               org-roam-dailies-find-tomorrow
               org-roam-dailies-find-date
               org-roam-tag-add
               org-roam-tag-delete)
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
      (spacemacs/declare-prefix-for-mode 'org-mode "mrd" "org-roam-dailies")
      (spacemacs/declare-prefix-for-mode 'org-mode "mrt" "org-roam-tags")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "rb" 'org-roam-switch-to-buffer
        "rdy" 'org-roam-dailies-find-yesterday
        "rdt" 'org-roam-dailies-find-today
        "rdT" 'org-roam-dailies-find-tomorrow
        "rdd" 'org-roam-dailies-find-date
        "rf" 'org-roam-find-file
        "rg" 'org-roam-graph
        "ri" 'org-roam-insert
        "rI" 'org-roam-insert-immediate
        "rl" 'org-roam-buffer-toggle-display
        "rta" 'org-roam-tag-add
        "rtd" 'org-roam-tag-delete))
    :config
    (spacemacs|hide-lighter org-roam-mode)
    ;; Make org-roam buffer sticky
    (setq org-roam-buffer-window-parameters '((no-delete-other-windows . t))
          org-roam-completion-system 'ivy
          org-roam-db-gc-threshold most-positive-fixnum
          org-roam-db-location (expand-file-name
                                "org-roam.db" spacemacs-cache-directory)
          org-roam-directory (expand-file-name "roam" org-directory)
          org-roam-verbose nil)))

(defun claude-org/init-org-roam-server ()
  (use-package org-roam-server
    :defer t
    :init
    (spacemacs/set-leader-keys "aors" 'org-roam-server-mode)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "rs" 'org-roam-server-mode)
    :config
    (setq org-roam-server-port 9000)))

(defun claude-org/post-init-plantuml-mode ()
  (with-eval-after-load 'plantuml-mode
    (cond
     ((spacemacs/system-is-mac)
      (setq plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar"
            plantuml-executable-path "/usr/local/bin/plantuml"))
     ((spacemacs/system-is-linux)
      (setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
            plantuml-executable-path "/usr/bin/plantuml")))
    (setq plantuml-default-exec-mode 'executable
          org-plantuml-jar-path plantuml-jar-path)))

(defun claude-org/init-valign ()
  "Properly align org tables that contain variable-pitch font,
CJK characters and images."
  (use-package valign
    :defer t
    :init
    (when claude--org-enable-valign
      (add-hook 'org-mode-hook 'valign-mode))
    (add-hook 'valign-mode-hook (lambda () (unless valign-mode
                                             (valign-remove-advice))))
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "Tv" 'valign-mode)))
