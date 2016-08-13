
;; Some general settings
(setq org-directory "~/Desktop/org")
(setq org-default-notes-file "~/Desktop/org/refile.org")
(setq org-default-diary-file "~/Desktop/org/diary.org")
(setq org-agenda-files (quote ("~/Desktop/org")))

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

;; == Capture Mode ==
(global-set-key (kbd "C-c c") 'org-capture)
;; Define the custum capture templates
(setq org-capture-templates
      (quote (("t" "todo" entry (file org-default-notes-file)
	       "* TODO %?\n%t\n%a\n" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file org-default-notes-file)
               "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t) )))


;; == Clocking Functions ==


;; == Agenda ==

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks t)

;; Compact the block agenda view
(setq org-agenda-compact-blocks nil)

;; Custom agenda command definitions

(defun gs/org-prefix-breadcrumb ()
    "Format"
    (concat "[" (org-format-outline-path (org-get-outline-path)) "]"))

(setq org-agenda-custom-commands
      '(("X" "Export Schedule" ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
					    (org-agenda-ndays 1)
					    (org-agenda-start-on-weekday nil)
					    (org-agenda-start-day "+0d")))
				(alltodo "" ((org-agenda-overriding-header "All Tasks:"))))
	 ((org-agenda-start-with-log-mode t)
	  (org-agenda-log-mode-items '(closed clock state))
	  (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t% s")
				      (timeline . "  % s")
				      (todo . "  %-12:c %(gs/org-prefix-breadcrumb) ")
				      (tags . "  %-12:c %(gs/org-prefix-breadcrumb) ")
				      (search . "  %i %-12:c")))
	 ))
	(" " "Agenda" ((agenda "") (alltodo))
	 ((org-agenda-ndays 10)
	  (org-agenda-start-on-weekday nil)
	  (org-agenda-start-day "-1d")
	  (org-agenda-start-with-log-mode t)
	  (org-agenda-log-mode-items '(closed clock state)))
	 )))
