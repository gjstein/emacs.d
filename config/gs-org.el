
(require 'org-agenda)

;;; Code:
;; Some general settings
(setq org-directory "~/org")
(setq org-default-notes-file "~/org/refile.org")
(defvar org-default-diary-file "~/org/diary.org")

;; Display properties
(setq org-cycle-separator-lines 0)
(setq org-tags-column 8)
(setq org-latex-prefer-user-labels t)

;; Show overview when open
(setq org-startup-folded t)

;; Dim blocked tasks (and other settings)
(setq org-enforce-todo-dependencies t)

;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %16TIMESTAMP_IA")

;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key 'expert)

;; Include the todo keywords
(setq org-fast-tag-selection-include-todo t)

;; == Custom State Keywords ==
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "|" "DONE(d)")
	(sequence "TASK(T)")
	(sequence "AMOTIVATOR(MA)" "TMOTIVATOR(MT)" "CMOTIVATOR(MC)" "|")
	(sequence "WAITING(w@/!)" "INACTIVE(i)" "SOMEDAY(s)" "|" "CANCELLED(c@/!)")))
;; Custom colors for the keywords
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("TASK" :foreground "#5C888B" :weight bold)
	("NEXT" :foreground "blue" :weight bold)
	("PROJ" :foreground "magenta" :weight bold)
	("AMOTIVATOR" :foreground "#F06292" :weight bold)
	("TMOTIVATOR" :foreground "#AB47BC" :weight bold)
	("CMOTIVATOR" :foreground "#5E35B1" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("WAITING" :foreground "orange" :weight bold)
	("INACTIVE" :foreground "magenta" :weight bold)
	("SOMEDAY" :foreground "cyan" :weight bold)
	("CANCELLED" :foreground "forest green" :weight bold)))
;; Auto-update tags whenever the state is changed
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
	("WAITING" ("SOMEDAY") ("INACTIVE") ("WAITING" . t))
	("INACTIVE" ("WAITING") ("SOMEDAY") ("INACTIVE" . t))
	("SOMEDAY" ("WAITING") ("INACTIVE") ("SOMEDAY" . t))
	(done ("WAITING") ("INACTIVE") ("SOMEDAY"))
	("TODO" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	("TASK" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	("NEXT" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	("PROJ" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	("AMOTIVATOR" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	("TMOTIVATOR" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	("CMOTIVATOR" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	("DONE" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))))

(defun gs/mark-next-done-parent-tasks-todo ()
  "Visit each parent task and change NEXT (or DONE) states to TODO."
  ;; Don't change the value if new state is "DONE"
  (let ((mystate (or (and (fboundp 'org-state)
                          (member state
				  (list "NEXT" "TODO")))
                     (member (nth 2 (org-heading-components))
			     (list "NEXT" "TODO")))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT" "DONE"))
            (org-todo "TODO")))))))
;; Note: I want to disable this for now
;; (add-hook 'org-after-todo-state-change-hook 'gs/mark-next-done-parent-tasks-todo 'append)

;; == Capture Mode Settings ==
;; Define the custum capture templates
(defvar org-capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
	 ("b" "Blank" entry (file org-default-notes-file)
	  "* %?\n%u")
	 ("m" "Meeting" entry (file org-default-notes-file)
	  "* Meeting with %? :MEETING:\n" :clock-in t :clock-resume t)
	 ("d" "Diary" entry (file+datetree "~/org/diary.org")
	  "* %?\n%U\n" :clock-in t :clock-resume t)
	 ("D" "Daily Log" entry (file "~/org/daily-log.org")
	  "* %u %?\n#+BEGIN: gjs-daily-clocktable :maxlevel 4 :date \"%u\" :link t :compact t \n#+END:\n\n*Jobs Done:*\n\n*Summary*: \n\n*Problem*: \n\n*Idea*: \n\n*Plan*: ")
	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%u" :clock-in t :clock-resume t)
	 ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	  "** NEXT %? \nDEADLINE: %t")
	 ("l" "Ledger entries")
	 ("lc" "MBNA" plain
	  (file "~/ledger/main-test.ledger")
	  "%(org-read-date) %^{Payee}
    ; %^{Comments/Tags}
    Expenses:%?  %^{Amount}
    Assets:Checking
")
	 ))

;; == Refile ==
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;;  Be sure to use the full path for refile setup
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; == Archive ==
(setq org-archive-location "archive/%s_archive::")
(defvar org-archive-file-header-format "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n")

;; == Habits ==
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 50)
(setq org-habit-show-habits-only-for-today t)

;; == Checklists ==
(require 'org-checklist)

;; == Org-ID ==
(require 'org-id)
;; I might also need org-ref

;;;; bh/helper-functions

(defun gs/is-project-p ()
  "A task with a 'PROJ' keyword"
  (member (nth 2 (org-heading-components)) '("PROJ")))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun gs/find-project-task ()
  "Any task with a todo keyword that is in a project subtree"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
	(when (member (nth 2 (org-heading-components)) '("PROJ"))
	  (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun gs/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (gs/find-project-task)
      (if (equal (point) task)
          nil t))))


(defun bh/find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

;; == Contacts ==
(require 'org-contacts)

(defun gs-store-org-headline ()
  (interactive)
  (when (and (eq major-mode 'org-mode)
             (org-at-heading-p))
    (org-store-link-props
     :type "file"
     :link (format "file:*%s" (nth 4 (org-heading-components)))
     :description (nth 4 (org-heading-components)))))

(defun gstest ()
  (interactive)
  ;; Just link to current headline
  (setq cpltxt (concat "file:"
		       (abbreviate-file-name
			(buffer-file-name (buffer-base-buffer)))))
  ;; Add a context search string
  (when t
    (let* ((element (org-element-at-point))
	   (name (org-element-property :name element)))
      (setq txt (cond
		 ((org-at-heading-p) nil)
		 (name)
		 ((org-region-active-p)
		  (buffer-substring (region-beginning) (region-end)))))
      (when (or (null txt) (string-match "\\S-" txt))
	(setq cpltxt
	      (concat cpltxt "::"
		      (condition-case nil
			  (org-make-org-heading-search-string txt)
			(error "")))
	      desc (or name
		       (nth 4 (ignore-errors (org-heading-components)))
		       "NONE")))))
  (when (string-match "::\\'" cpltxt)
    (setq cpltxt (substring cpltxt 0 -2)))
  (setq link cpltxt)
  link
  )



(defun gs-helm-org-link-to-contact ()
  (interactive)
  (if (eq major-mode 'org-mode)
      (let ((temp-point (point))
	    (temp-buffer (current-buffer))
	    (org-refile-targets (quote (("~/org/contacts.org" :level . 2))))
	   ;; (org-refile-targets (quote ((("~/org/contacts.org")) :maxlevel . 9)))
	    )
	(org-refile '(4))
	(let ((link-text (gstest))
	      (desc-text (nth 4 (org-heading-components))))
	       ;(concat "[[file:contacts.org::" (nth 4 (org-heading-components)) "]]")))
	  (unless (eq (current-buffer) temp-buffer) (switch-to-buffer temp-buffer))
	  (goto-char temp-point)
	  (insert (concat "[[" link-text "][" desc-text "]]")
	  )
	))
    (user-error "This function is meant to be called within org")
    ))

;; == clocking Functions ==
(require 'org-clock)

;; If not a project, clocking-in changes TODO to NEXT
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (not (bh/is-project-p)))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(add-hook 'org-mode-hook
    (lambda ()
      (define-key org-mode-map (kbd "C-c C-.") 'org-time-stamp-inactive)))

;; Also ensure that NEXT projects are switched to TODO when clocking in
(add-hook 'org-clock-in-hook 'gs/mark-next-done-parent-tasks-todo 'append)

;; == Agenda ==

(load-file "~/.emacs.d/config/gs-org-agenda.el")

;; === Custom Clocktable ===
(require 'org-clock)
(defun gjs-org-clocktable-filter-empty-tables (ipos tables params)
  "Removes all empty tables before printing the clocktable"
  (org-clocktable-write-default ipos
				(seq-filter
				 (lambda (tbl)
				   (not (null (nth 2 tbl))))
				 tables)
				params)
  )

(defun org-dblock-write:gjs-daily-clocktable (params)
  "Custom clocktable command for my daily log"
  (let ((local-params params)
	(date-str
	 (if (plist-get params ':date)
	 (substring
		   (plist-get params ':date)
		   1 11)))
	)
    (plist-put params ':block date-str)
    (plist-put params ':formatter 'gjs-org-clocktable-filter-empty-tables)
    (plist-put params ':scope 'agenda)
    (org-dblock-write:clocktable params)
    )
  )

(provide 'gs-org)
;;; gs-org.el ends here
