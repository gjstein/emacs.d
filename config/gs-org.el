
(require 'org-agenda)

;; Some general settings
(setq org-directory "~/Desktop/org")
(setq org-default-notes-file "~/Desktop/org/refile.org")
(setq org-default-diary-file "~/Desktop/org/diary.org")
(setq org-agenda-files (quote ("~/Desktop/org")))

(setq org-tags-column 100)
(setq org-agenda-tags-column org-tags-column)

;; Custom TODO commands
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("NEXT" :foreground "blue" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("WAITING" :foreground "orange" :weight bold)
	("HOLD" :foreground "magenta" :weight bold)
	("CANCELLED" :foreground "forest green" :weight bold)
	("MEETING" :foreground "forest green" :weight bold)
	("PHONE" :foreground "forest green" :weight bold)))
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-custom-properties '("LOCATION"))

;; == Capture Mode ==
;; Define the custum capture templates
(setq org-capture-templates
      (quote (("t" "todo" entry (file org-default-notes-file)
	       "* TODO %?\n%t\n%a\n" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file org-default-notes-file)
               "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t) )))

;; == Refile ==
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; == Clocking Functions ==


;; == Agenda ==

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view (disabled)
(setq org-agenda-compact-blocks nil)


;; Some helper functions for agenda views
(defun gs/org-agenda-prefix-breadcrumb ()
    "Format"
    (concat "[" (org-format-outline-path (org-get-outline-path)) "]"))
(defun gs/org-agenda-add-location-string ()
  "Gets the value of the LOCATION property"
  (let ((loc (org-entry-get (point) "LOCATION")))
    (if (> (length loc) 0)
	(concat "\n" (make-string 15 ?\s) "Location: " loc
		"\n" (make-string 15 ?\s))
      "")))
;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '((" " "Export Schedule" ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
					    (org-agenda-ndays 1)
					    (org-agenda-start-on-weekday nil)
					    (org-agenda-start-day "+0d")))
				(tags "REFILE" ((org-agenda-overriding-header "Tasks to Refile:")
						(org-tags-match-list-sublevels nil)))
				(agenda "" ((org-agenda-overriding-header "Week Schedule")
					    (org-agenda-ndays 10)
					    (org-agenda-start-day"-1d")))
				(tags-todo "-REFILE" ((org-agenda-overriding-header "All Tasks:"))))
	 ((org-agenda-start-with-log-mode t)
	  (org-agenda-log-mode-items '(closed clock state))
	  (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
				      (timeline . "  % s")
				      (todo . "  %-12:c %(gs/org-agenda-prefix-breadcrumb) ")
				      (tags . "  %-12:c %(gs/org-agenda-prefix-breadcrumb) ")
				      (search . "  %i %-12:c")))
	 ))
	("X" "Agenda" ((agenda "") (alltodo))
	 ((org-agenda-ndays 10)
	  (org-agenda-start-on-weekday nil)
	  (org-agenda-start-day "-1d")
	  (org-agenda-start-with-log-mode t)
	  (org-agenda-log-mode-items '(closed clock state)))
	 )))
