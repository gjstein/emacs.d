;;; gs-org-agenda.el --- Customizations/extensions for org-agenda

;; Copyright (C) 2019 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 18 Jan 2019

;; Keywords: configuration, org
;; Homepage: https://github.com/gjstein/emacs.d

;;; Commentary:


;;; Code:

(require 'org-agenda)



;;;; General Agenda Settings

(setq org-agenda-files (quote ("~/org" "~/org/archive")))
(setq org-agenda-tags-column org-tags-column)
(setq org-agenda-sticky t)
(setq org-agenda-inhibit-startup nil)
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view (disabled)
(setq org-agenda-compact-blocks nil)

;; Set the times to display in the time grid
(setq org-agenda-time-grid
      (quote
       ((daily today remove-match)
        (800 1200 1600 2000)
        "......" "----------------")))

;; Variables for ignoring tasks with deadlines
(defvar gs/hide-deadline-next-tasks t)
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-deadline-warning-days 10)

;;;; Task and project filter functions
; Some helper functions for selection within agenda views

(defun gs/select-with-tag-function (select-fun-p)
  (save-restriction
    (widen)
    (let ((next-headline
	   (save-excursion (or (outline-next-heading)
			       (point-max)))))
      (if (funcall select-fun-p) nil next-headline))))

(defun gs/select-projects ()
  "Selects tasks which are project headers"
  (gs/select-with-tag-function #'gs/is-project-p))

(defun gs/select-project-tasks ()
  "Skips tags which belong to projects (and is not a project itself)"
  (gs/select-with-tag-function
   #'(lambda () (and
		 (not (gs/is-project-p))
		 (gs/is-project-subtree-p)))))

(defun gs/select-standalone-tasks ()
  "Skips tags which belong to projects. Is neither a project, nor does it blong to a project"
  (gs/select-with-tag-function
   #'(lambda () (and
		 (not (gs/is-project-p))
		 (not (gs/is-project-subtree-p))))))

(defun gs/select-projects-and-standalone-tasks ()
  "Skips tags which are not projects"
  (gs/select-with-tag-function
   #'(lambda () (or
		 (gs/is-project-p)
		 (gs/is-project-subtree-p)))))

(defun gs/org-agenda-project-warning ()
  "Is a project stuck or waiting. If the project is not stuck,
show nothing. However, if it is stuck and waiting on something,
show this warning instead."
  (if (gs/org-agenda-project-is-stuck)
    (if (gs/org-agenda-project-is-waiting) " !W" " !S") ""))

(defun gs/org-agenda-project-is-stuck ()
  "Is a project stuck"
  (if (gs/is-project-p) ; first, check that it's a project
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
	     (has-next))
	(save-excursion
	  (forward-line 1)
	  (while (and (not has-next)
		      (< (point) subtree-end)
		      (re-search-forward "^\\*+ NEXT " subtree-end t))
	    (unless (member "WAITING" (org-get-tags-at))
	      (setq has-next t))))
	(if has-next nil t)) ; signify that this project is stuck
    nil)) ; if it's not a project, return an empty string

(defun gs/org-agenda-project-is-waiting ()
  "Is a project stuck"
  (if (gs/is-project-p) ; first, check that it's a project
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
	(save-excursion
	  (re-search-forward "^\\*+ WAITING" subtree-end t)))
    nil)) ; if it's not a project, return an empty string

;; Some helper functions for agenda views
(defun gs/org-agenda-prefix-string ()
  "Format"
  (let ((path (org-format-outline-path (org-get-outline-path))) ; "breadcrumb" path
	(stuck (gs/org-agenda-project-warning))) ; warning for stuck projects
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

;;;; Agenda block definitions

(defvar gs-org-agenda-block--today-schedule
  '(agenda "" ((org-agenda-overriding-header "Today's Schedule:")
	       (org-agenda-span 'day)
	       (org-agenda-ndays 1)
	       (org-agenda-start-on-weekday nil)
	       (org-agenda-start-day "+0d")))
  "A block showing a 1 day schedule.")

(defvar gs-org-agenda-block--weekly-log
  '(agenda "" ((org-agenda-overriding-header "Weekly Log")))
  "A block showing my schedule and logged tasks for this week.")

(defvar gs-org-agenda-block--previous-calendar-data
  '(agenda "" ((org-agenda-overriding-header "Previous Calendar Data (last 3 weeks)")
	       (org-agenda-start-day "-21d")
	       (org-agenda-span 21)
	       (org-agenda-start-on-weekday nil)))
  "A block showing my schedule and logged tasks for the last few weeks.")

(defvar gs-org-agenda-block--upcoming-calendar-data
  '(agenda "" ((org-agenda-overriding-header "Upcoming Calendar Data (next 2 weeks)")
	       (org-agenda-start-day "0d")
	       (org-agenda-span 14)
	       (org-agenda-start-on-weekday nil)))
  "A block showing my schedule for the next couple weeks.")

(defvar gs-org-agenda-block--refile
  '(tags "REFILE-ARCHIVE-REFILE=\"nil\"|INFO"
	 ((org-agenda-overriding-header "Headings needing refiling or other info:")
	  (org-tags-match-list-sublevels nil)))
  "Headings needing refiling or other info.")

(defvar gs-org-agenda-block--next-tasks
  '(tags-todo "-INACTIVE-SOMEDAY-CANCELLED-ARCHIVE/!NEXT"
	      ((org-agenda-overriding-header "Next Tasks:")
	       ))
  "Next tasks.")

(defvar gs-org-agenda-block--active-projects
  '(tags-todo "-INACTIVE-SOMEDAY-CANCELLED-REFILEr/!"
	      ((org-agenda-overriding-header "Active Projects:")
	       (org-agenda-skip-function 'gs/select-projects)))
  "All active projects: no inactive/someday/cancelled/refile.")

(defvar gs-org-agenda-block--standalone-tasks
  '(tags-todo "-INACTIVE-SOMEDAY-CANCELLED-REFILE-ARCHIVE-STYLE=\"habit\"/!-NEXT"
	      ((org-agenda-overriding-header "Standalone Tasks:")
	       (org-agenda-skip-function 'gs/select-standalone-tasks)))
  "Tasks (TODO) that do not belong to any projects.")

(defvar gs-org-agenda-block--waiting-tasks
  '(tags-todo "-INACTIVE-SOMEDAY-CANCELLED-ARCHIVE/!WAITING"
	     ((org-agenda-overriding-header "Waiting Tasks:")
	      ))
  "Tasks marked as waiting.")

(defvar gs-org-agenda-block--remaining-project-tasks
  '(tags-todo "-INACTIVE-SOMEDAY-CANCELLED-WAITING-REFILE-ARCHIVE/!-NEXT"
	      ((org-agenda-overriding-header "Remaining Project Tasks:")
	       (org-agenda-skip-function 'gs/select-project-tasks)))
  "Non-NEXT TODO items belonging to a project.")

(defvar gs-org-agenda-block--inactive-tags
  '(tags-todo "-SOMEDAY-ARCHIVE-CANCELLED/!INACTIVE"
	 ((org-agenda-overriding-header "Inactive Projects and Tasks")
	  (org-tags-match-list-sublevels nil)))
  "Inactive projects and tasks.")

(defvar gs-org-agenda-block--someday-tags
  '(tags-todo "-INACTIVE-ARCHIVE-CANCELLED/!SOMEDAY"
	 ((org-agenda-overriding-header "Someday Projects and Tasks")
	  (org-tags-match-list-sublevels nil)))
  "Someday projects and tasks.")

(defvar gs-org-agenda-block--motivators
  '(todo "AMOTIVATOR|TMOTIVATOR|CMOTIVATOR"
	 ((org-agenda-overriding-header "Motivators (Active/Tangible/Conceptual)")))
  "All my 'motivators' across my projects.")

(defvar gs-org-agenda-block--end-of-agenda
  '(tags "ENDOFAGENDA"
	 ((org-agenda-overriding-header "End of Agenda")
	  (org-tags-match-list-sublevels nil)))
  "End of the agenda.")

(defvar gs-org-agenda-display-settings
  '((org-agenda-start-with-log-mode t)
    (org-agenda-log-mode-items '(clock))
    (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
				(timeline . "  % s")
				(todo . "  %-12:c %(gs/org-agenda-prefix-string) ")
				(tags . "  %-12:c %(gs/org-agenda-prefix-string) ")
				(search . "  %i %-12:c")))
    (org-agenda-todo-ignore-deadlines 'near)
    (org-agenda-todo-ignore-scheduled t))
  "Display settings for my agenda views.")

(defvar gs-org-agenda-entry-display-settings
  '(,gs-org-agenda-display-settings
    (org-agenda-entry-text-mode t))
  "Display settings for my agenda views with entry text.")

;;;; Agenda Definitions

(setq org-agenda-custom-commands
      `(("h" "Habits" agenda "STYLE=\"habit\""
	 ((org-agenda-overriding-header "Habits")
	  (org-agenda-sorting-strategy
	   '(todo-state-down effort-up category-keep))))
	(" " "Export Schedule"
	 (,gs-org-agenda-block--today-schedule
	  ,gs-org-agenda-block--refile
	  ,gs-org-agenda-block--next-tasks
	  ,gs-org-agenda-block--active-projects
	  ,gs-org-agenda-block--end-of-agenda)
	 ,gs-org-agenda-display-settings)
	("L" "Weekly Log"
	 (,gs-org-agenda-block--weekly-log)
	 ,gs-org-agenda-display-settings)
	("r " "Agenda Review (all)"
	 (,gs-org-agenda-block--next-tasks
	  ,gs-org-agenda-block--refile
	  ,gs-org-agenda-block--active-projects
	  ,gs-org-agenda-block--standalone-tasks
	  ,gs-org-agenda-block--waiting-tasks
	  ,gs-org-agenda-block--remaining-project-tasks
	  ,gs-org-agenda-block--inactive-tags
	  ,gs-org-agenda-block--someday-tags
	  ,gs-org-agenda-block--motivators
	  ,gs-org-agenda-block--end-of-agenda)
	 ,gs-org-agenda-display-settings)
	("rn" "Agenda Review (next tasks)"
	 (,gs-org-agenda-block--next-tasks
	  ,gs-org-agenda-block--end-of-agenda)
	 ,gs-org-agenda-display-settings)
	("rp" "Agenda Review (previous calendar data)"
	 (,gs-org-agenda-block--previous-calendar-data
	  ,gs-org-agenda-block--end-of-agenda)
	 ,gs-org-agenda-display-settings)
	("ru" "Agenda Review (upcoming calendar data)"
	 (,gs-org-agenda-block--upcoming-calendar-data
	  ,gs-org-agenda-block--end-of-agenda)
	 ,gs-org-agenda-display-settings)
	("rw" "Agenda Review (waiting tasks)"
	 (,gs-org-agenda-block--waiting-tasks
	  ,gs-org-agenda-block--end-of-agenda)
	 ,gs-org-agenda-display-settings)
	("rP" "Agenda Review (projects list)"
	 (,gs-org-agenda-block--active-projects
	  ,gs-org-agenda-block--end-of-agenda)
	 ,gs-org-agenda-display-settings)
	("ri" "Agenda Review (someday and inactive projects/tasks)"
	 (,gs-org-agenda-block--someday-tags
	  ,gs-org-agenda-block--inactive-tags
	  ,gs-org-agenda-block--end-of-agenda)
	 ,gs-org-agenda-display-settings)
	("rm" "Agenda Review (motivators)"
	 (,gs-org-agenda-block--motivators
	  ,gs-org-agenda-block--end-of-agenda)
	 ,gs-org-agenda-entry-display-settings)
	))


;;;; Agenda Navigation

;; Search for a "=" and go to the next line
(defun gs/org-agenda-next-section ()
  "Go to the next section in an org agenda buffer."
  (interactive)
  (if (search-forward "===" nil t 1)
      (forward-line 1)
    (goto-char (point-max)))
  (beginning-of-line))

;; Search for a "=" and go to the previous line
(defun gs/org-agenda-prev-section ()
  "Go to the next section in an org agenda buffer."
  (interactive)
  (forward-line -2)
  (if (search-forward "===" nil t -1)
      (forward-line 1)
    (goto-char (point-min))))

;;;; Agenda Post-processing

;; Highlight the "!!" for stuck projects (for emphasis)
(defun gs/org-agenda-project-highlight-warning ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "!W" nil t)
      (progn
	(add-face-text-property
	 (match-beginning 0) (match-end 0)
	 '(bold :foreground "orange"))
	))
    (goto-char (point-min))
    (while (re-search-forward "!S" nil t)
      (progn
	(add-face-text-property
	 (match-beginning 0) (match-end 0)
	 '(bold :foreground "white" :background "red"))
	))
    (goto-char (point-min))
    (while (re-search-forward ":OPT:" nil t)
      (progn
	(put-text-property
	 (+ 14 (point-at-bol)) (match-end 0)
	 'face 'font-lock-comment-face)  ; also 'org-time-grid
	))
    (goto-char (point-min))
    (while (re-search-forward ":TENT:" nil t)
      (progn
	(put-text-property
	 (+ 14 (point-at-bol)) (match-end 0)
	 'face 'font-lock-comment-face)
	))
    ))
(add-hook 'org-agenda-finalize-hook 'gs/org-agenda-project-highlight-warning)

;; Remove empty agenda blocks
(defun gs/remove-agenda-regions ()
  (save-excursion
    (goto-char (point-min))
    (let ((region-large t))
      (while (and (< (point) (point-max)) region-large)
	(set-mark (point))
	(gs/org-agenda-next-section)
	(if (< (- (region-end) (region-beginning)) 5) (setq region-large nil)
	  (if (< (count-lines (region-beginning) (region-end)) 4)
	      (delete-region (region-beginning) (region-end)))
	  )))))
(add-hook 'org-agenda-finalize-hook 'gs/remove-agenda-regions)


(provide 'gs-org-agend)
;;; gs-org-agenda.el ends here
