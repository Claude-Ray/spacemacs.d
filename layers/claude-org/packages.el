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
    (valign :location (recipe
                       :fetcher github
                       :repo "casouri/valign"))
    ))

(defun claude-org/post-init-org ()

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n!)" "WAIT(w@/!)" "|" "DONE(d)" "CANCELED(c@)")))

  (setq org-agenda-files (list "~/org"))

  ;; Only show today's task in overview display.
  (setq org-agenda-span 'day)

  (setq org-agenda-start-with-log-mode t)

  (setq org-agenda-log-mode-items '(closed clock state))

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
           "* TODO %?\n %i\n %a")
          ("w" "Work" entry (file+headline "~/org/work.org" "Works")
           "* TODO %?\n %i\n %U")
          ("i" "Idea" entry (file+headline "~/org/note.org" "Ideas")
           "* TODO %?\n %i\n %U")
          ("m" "Mark" entry (file+headline "~/org/note.org" "Marks")
           "* %?\n %i\n %U %a")
          ("n" "Note" entry (file+headline "~/org/note.org" "Notes")
           "* %?\n %i\n %U")
          ("j" "Journal" entry (file+headline "~/org/journal.org" "Journals")
           "* %?\n %i\n %U"
           :empty-lines 1)
          ))

  ;; Targets include this file and any file contributing to the agenda - up to 3 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 3)
                                   (org-agenda-files :maxlevel . 2))))

  ;; Use full outline paths for refile targets - we file directly with IDO
  (setq org-refile-use-outline-path 'file)

  ;; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)

  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Exclude DONE state tasks from refile targets
  (setq org-refile-target-verify-function 'claude-org//verify-refile-target)

  (setq org-archive-location "%s_archive::* Archived Tasks"))

(defun claude-org/init-valign ()
  "Properly align org tables that contain variable-pitch font,
CJK characters and images."
  (use-package valign
    :hook (org-mode . valign-mode)))
