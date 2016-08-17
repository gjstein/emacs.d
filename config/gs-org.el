
(require 'org-agenda)

;;; Code:
;; Some general settings
(setq org-directory "~/org")
(setq org-default-notes-file "~/org/refile.org")
(defvar org-default-diary-file "~/org/diary.org")
(setq org-agenda-files (quote ("~/org")))

;; Display properties
(setq org-cycle-separator-lines 0)
(setq org-tags-column 80)
(setq org-agenda-tags-column org-tags-column)

;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %16TIMESTAMP_IA")

;; == Custom State Keywords ==
(setq org-use-fast-todo-selection t)
(setq org-fast-tag-selection-single-key 'expert)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	(sequence "WAITING(w@/!)" "INACTIVE(i@/!)" "|" "CANCELLED(c@/!)" "MEETING")))
;; Custom colors for the keywords
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("NEXT" :foreground "blue" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("WAITING" :foreground "orange" :weight bold)
	("INACTIVE" :foreground "magenta" :weight bold)
	("CANCELLED" :foreground "forest green" :weight bold)
	("MEETING" :foreground "forest green" :weight bold)))
;; Auto-update tags whenever the state is changed
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
	("WAITING" ("WAITING" . t))
	("INACTIVE" ("WAITING") ("INACTIVE" . t))
	(done ("WAITING") ("INACTIVE"))
	("TODO" ("WAITING") ("CANCELLED") ("INACTIVE"))
	("NEXT" ("WAITING") ("CANCELLED") ("INACTIVE"))
	("DONE" ("WAITING") ("CANCELLED") ("INACTIVE"))))

;; == Capture Mode Settings ==
;; Define the custum capture templates
(setq org-capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
	 ("m" "Meeting" entry (file org-default-notes-file)
	  "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
	 ("d" "Diary" entry (file+datetree "~/org/diary.org")
	  "* %?\n%U\n" :clock-in t :clock-resume t)
	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
	 ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	  "** NEXT %? \nDEADLINE: %t") ))

;; == Refile ==
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;;  Be sure to use the full path for refile setup
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; == Habits ==
(require 'org-habit)
(setq org-modules '(org-habit))
(setq org-habit-show-habits-only-for-today t)

;; == Clocking Functions ==

(add-hook 'org-mode-hook
    (lambda ()
        (define-key org-mode-map (kbd "C-c C-.") 'org-time-stamp-inactive)))

;; == Agenda ==

;; Dim blocked tasks
(setq org-agenda-dim-blocked-tasks t)

;; Compact the block agenda view (disabled)
(setq org-agenda-compact-blocks nil)

;; Set the times to display in the time grid
(setq org-agenda-time-grid
  '((daily today require-timed)
    "----------------"
    (800 1200 1600 2000)))

;; bh/helper-functions
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
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
(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
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

;; Some helper functions for selection within agenda views
(defun gs/select-with-tag-function (select-fun-p)
  (save-restriction
    (widen)
    (let ((next-headline
	   (save-excursion (or (outline-next-heading)
			       (point-max)))))
      (if (funcall select-fun-p) nil next-headline))))
  
(defun gs/select-projects ()
  "Selects tasks which are project headers"
  (gs/select-with-tag-function #'bh/is-project-p))
(defun gs/select-project-tasks ()
  "Skips tags which belong to projects (and is not a project itself)"
  (gs/select-with-tag-function
   #'(lambda () (and
		 (not (bh/is-project-p))
		 (bh/is-project-subtree-p)))))
(defun gs/select-standalone-tasks ()
  "Skips tags which belong to projects. Is neither a project, nor does it blong to a project"
  (gs/select-with-tag-function
   #'(lambda () (and
		 (not (bh/is-project-p))
		 (not (bh/is-project-subtree-p))))))
(defun gs/select-projects-and-standalone-tasks ()
  "Skips tags which are not projects"
  (gs/select-with-tag-function
   #'(lambda () (or
		 (bh/is-project-p)
		 (bh/is-project-subtree-p)))))


(defun gs/org-agenda-project-stuck-warning ()
  "Is a project stuck"
  (if (bh/is-project-p) ; first, check that it's a project
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
	     (has-next))
	(save-excursion
	  (forward-line 1)
	  (while (and (not has-next)
		      (< (point) subtree-end)
		      (re-search-forward "^\\*+ NEXT " subtree-end t))
	    (unless (member "WAITING" (org-get-tags-at))
	      (setq has-next t))))
	(if has-next "" " !!")) ; signify that this project is stuck
    "")) ; if it's not a project, return an empty string

;; Highlight the "!!" for stuck projects (for emphasis)
(defun gs/org-finalize-agenda-hook ()
  (save-excursion
	      (goto-char (point-min))
	      (while (re-search-forward "!!" nil t)
		(progn
		  (add-face-text-property
		   (match-beginning 0) (match-end 0)
		   '(bold :foreground "white" :background "red"))
		  )	
	      )))
(add-hook 'org-finalize-agenda-hook 'gs/org-finalize-agenda-hook)

;; Some helper functions for agenda views
(defun gs/org-agenda-prefix-string ()
  "Format"
  (let ((path (org-format-outline-path (org-get-outline-path))) ; "breadcrumb" path
	(stuck (gs/org-agenda-project-stuck-warning))) ; warning for stuck projects
       (if (> (length path) 0)
	   (concat stuck ; add stuck warning
		   " [" path "]") ; add "breadcrumb"
	 stuck)))
       
(defun gs/org-agenda-add-location-string ()
  "Gets the value of the LOCATION property"
  (let ((loc (org-entry-get (point) "LOCATION")))
    (if (> (length loc) 0)
	(concat "{" loc "} ")
      "")))

;; Variables for ignoring tasks with deadlines
(defvar gs/hide-deadline-next-tasks t)
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-deadline-warning-days 10)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("h" "Habits" agenda "STYLE=\"habit\""
	 ((org-agenda-overriding-header "Habits")
	  (org-agenda-sorting-strategy
	   '(todo-state-down effort-up category-keep))))
	(" " "Export Schedule" ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
					    (org-agenda-ndays 1)
					    (org-agenda-start-on-weekday nil)
					    (org-agenda-start-day "+0d")
					    (org-agenda-todo-ignore-deadlines nil)))
				(tags-todo "-CANCELLED/!NEXT"
					   ((org-agenda-overriding-header "Next Tasks:")
					    ))
				(tags "REFILE-REFILE=\"nil\""
				      ((org-agenda-overriding-header "Tasks to Refile:")
				       (org-tags-match-list-sublevels nil)))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILEr/!"
					   ((org-agenda-overriding-header "Active Projects")
					    (org-agenda-skip-function 'gs/select-projects)))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE/!-NEXT"
					   ((org-agenda-overriding-header "Remaining Project Tasks")
					    (org-agenda-skip-function 'gs/select-project-tasks)))
				(tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-STYLE=\"habit\"/!"
					   ((org-agenda-overriding-header "Standalone Tasks")
					    (org-agenda-skip-function 'gs/select-standalone-tasks)))
				(agenda "" ((org-agenda-overriding-header "Week At A Glance")
					    (org-agenda-ndays 5)
					    (org-agenda-start-day"+1d")
					    (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %s [%b] ")))))
				(tags-todo "-CANCELLED-REFILE" ((org-agenda-overriding-header "All Tasks:"))))
	 ((org-agenda-start-with-log-mode t)
	  (org-agenda-log-mode-items '(clock))
	  (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
				      (timeline . "  % s")
				      (todo . "  %-12:c %(gs/org-agenda-prefix-string) ")
				      (tags . "  %-12:c %(gs/org-agenda-prefix-string) ")
				      (search . "  %i %-12:c")))
	  (org-agenda-todo-ignore-deadlines 'near)
	  (org-agenda-todo-ignore-scheduled t)))
	("X" "Agenda" ((agenda "") (alltodo))
	 ((org-agenda-ndays 10)
	  (org-agenda-start-on-weekday nil)
	  (org-agenda-start-day "-1d")
	  (org-agenda-start-with-log-mode t)
	  (org-agenda-log-mode-items '(closed clock state)))
	 )))

;; == Agenda Navigation ==

;; Search for a "=" and go to the next line
(defun gs/org-agenda-next-section ()
  "Go to the next section in an org agenda buffer"
  (interactive)
  (if (search-forward (char-to-string ?=) nil t 1)
      (forward-line 1)
    (goto-char (point-max)))
  (beginning-of-line))

;; Search for a "=" and go to the previous line
(defun gs/org-agenda-prev-section ()
  "Go to the next section in an org agenda buffer"
  (interactive)
  (forward-line -2)
  (if (search-forward (char-to-string ?=) nil t -1)
      (forward-line 1)
    (goto-char (point-min))))

;; Bind the keys
(add-hook 'org-agenda-mode-hook
	  (lambda ()
	    (define-key org-agenda-mode-map (kbd "M-n") 'gs/org-agenda-next-section)
	    (define-key org-agenda-mode-map (kbd "M-p") 'gs/org-agenda-prev-section)))

(provide 'gs-org)
;;; gs-org.el ends here
