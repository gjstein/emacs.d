;;; init-30-doc-org.el --- Code for initializing org-mode

;; Copyright (C) 2017 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 20 Aug 2015

;; Keywords: configuration, org
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:
;; Runs org-mode along with some custom configuration files
;;
;; More information can be found at:
;;    http://doc.norang.ca/org-mode.html

;;; Code:


(use-package org
  :ensure t :ensure org-contacts :ensure org-contrib
  :init
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (setq org-mobile-directory "~/Dropbox (Personal)/Apps/MobileOrg")
  :diminish visual-line-mode
  :diminish org-indent-mode
  :defer t
  :bind (("\C-c a" . org-agenda)
	 ("\C-c c" . org-capture)
	 ("\C-c j" . gs-helm-org-link-to-contact)
	 ("\C-c \C-]" . org-ref-cite-insert-helm)
	 )
  :config

  ;; Expansion for blocks "<s" -> "#+BEGIN_SRC"
  (require 'org-tempo)

  (defun sk/diminish-org-indent ()
    "Diminish org-indent-mode on the modeline"
    (interactive)
    (diminish 'org-indent-mode ""))
  (add-hook 'org-indent-mode-hook #'sk/diminish-org-indent)

  ;; Fix evil-auto-indent for org buffers.
  (defun gs-org-disable-evil-auto-indent nil
    "Disables evil's auto-indent for org."
    (setq evil-auto-indent nil)
    )
  (add-hook 'org-mode-hook #'gs-org-disable-evil-auto-indent)
  
  ;; Custom functions for emacs & org mode
  (load-file "~/.emacs.d/config/gs-org.el")

  (setq org-enforce-todo-dependencies nil)
  (setq org-display-inline-images t)
  (setq org-redisplay-inline-images t)
  (setq org-startup-with-inline-images "inlineimages")

  ;; == Agenda ==
  (defvar org-agenda-window-setup)
  (setq org-agenda-window-setup 'current-window)

  ;; Run/highlight code using babel in org-mode

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     ;; (ipython . t)
     (python . t)
     (octave . t)
     (C . t)
     (shell . t)
     ))

  ;; Syntax hilight in #+begin_src blocks
  (setq org-src-fontify-natively t)
  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil)
  ;; Display inline images after running code
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  ;; Don't indent code after editing
  (setq org-edit-src-content-indentation 0)

  ;; Org + LaTeX
  (with-eval-after-load 'ox-latex
    (setq org-latex-prefer-user-labels t)
    (setq org-latex-listings 'minted)
    (setq org-latex-custom-lang-environments
	  '(
	    (emacs-lisp "common-lispcode")
	    ))
    (setq org-latex-minted-options
	  '(("frame" "lines")
	    ("fontsize" "\\scriptsize")
	    ("linenos" "")))
    (add-to-list 'org-latex-minted-langs '(ipython "python"))

    ;; Ensure LaTeX export correctly produces the bibliography
    ;; (setq org-latex-pdf-process
    ;; 	'("pdflatex -interaction nonstopmode -output-directory %o %f"
    ;; 	  "bibtex %b"
    ;; 	  "pdflatex -interaction nonstopmode -output-directory %o %f"
    ;; 	  "pdflatex -interaction nonstopmode -output-directory %o %f"))
    ;; )
    (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

    (add-to-list 'org-latex-classes
	       '("book-noparts"
		 "\\documentclass[12pt,oneside]{book}"
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
	       '("mitthesis"
		 "\\documentclass[12pt,twoside]{mitthesis}"
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
	       '("annotatedbib"
		 "\\documentclass[10pt,oneside]{article}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")))
    (add-to-list 'org-latex-classes
	       '("ieeetran"
		 "\\documentclass{IEEEtran}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")))
    (add-to-list 'org-latex-classes
	       '("ieeeconf"
		 "\\documentclass{ieeeconf}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")))

  (require 'ox-beamer)

  (setq org-highlight-latex-and-related '(latex))


  (use-package helm-org-rifle
    :ensure t)

  ;; Capture mode
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (general-define-key
   :keymaps 'org-capture-mode-map
   :states '(normal motion)
   :prefix gjs-leader-key
   "c" 'org-capture-finalize
   "k" 'org-capture-kill
   "w" 'org-capture-refile
   )

  ;; Evil key configurations (agenda)
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (defvar org-agenda-mode-map)
  (general-define-key
   :keymaps 'org-agenda-mode-map
   :states '(normal motion)
   "l" 'org-agenda-later
   "h" 'org-agenda-earlier
   "j" 'org-agenda-next-line
   "k" 'org-agenda-previous-line
   (kbd "RET") 'org-agenda-switch-to
   [escape] 'org-agenda-quit
   "q" 'org-agenda-quit
   "s" 'org-save-all-org-buffers
   "t" 'org-agenda-todo
   "T" 'org-agenda-set-tags
   "g" 'org-agenda-redo
   "v" 'org-agenda-view-mode-dispatch
   "." 'org-agenda-goto-today
   "J" 'gs/org-agenda-next-section
   "K" 'gs/org-agenda-prev-section
   "c" 'org-agenda-goto-calendar
   "i" 'org-agenda-clock-in
   "o" 'org-agenda-clock-out
   "E" 'org-agenda-entry-text-mode
   )
  (general-define-key
   :keymaps 'org-agenda-mode-map
   :prefix gjs-leader-key
   :states '(normal motion)
   "" '(:ignore t :which-key "Agenda")
   "i" 'org-agenda-clock-in
   "k" 'org-agenda-kill
   "o" 'org-agenda-clock-out
   "t" 'org-agenda-todo
   "w" 'org-agenda-refile
   "/" 'org-agenda-filter-by-tag
   "cs" '(gs-org-goto :which-key "org goto")
   "c/" '(helm-org-rifle :which-key "org-rifle")
   "]" 'org-ref-cite-insert-helm
   )


  ;; Evil key configuration (org)

  (defun gs-org-meta-return (&optional _arg)
    "Ensures org-meta-return switches to evil insert mode"
    (interactive)
    (evil-append 0)
    (org-meta-return _arg)
    )
 
  (defun gs-org-insert-heading-respect-content (&optional invisible-ok)
    "Insert heading with `org-insert-heading-respect-content' set to t."
    (interactive)
    (org-insert-heading '(4) invisible-ok)
    (evil-insert 0))

  (defun gs-org-goto ()
    "Insert heading with `org-insert-heading-respect-content' set to t."
    (interactive)
    (org-refile '(4))
    ;; (let ((org-goto-interface 'outline-path-completion)) (org-goto))
    )

  ;; (general-define-key
  ;;  :keymaps org-mode-map
  ;;  :states '(normal)
  ;;  (kbd "<M-return>") 'gs-org-meta-return
  ;;  (kbd "<C-return>") 'gs-org-insert-heading-respect-content
  ;;  )
  (general-define-key
   :prefix (cl-concatenate 'string gjs-leader-key)
   :keymaps 'org-mode-map
   :states '(normal motion)
   "i" '(org-clock-in :which-key "clock in")
   "o" '(org-clock-out :which-key "clock out")
   "t" '(org-todo :which-key "todo state")
   "ct" '(org-todo :which-key "todo state")
   "ce" '(org-export-dispatch :which-key "org export")
   "cp" '(org-set-property :which-key "org set property")
   "cs" '(gs-org-goto :which-key "org goto")
   "cj" '(gs-helm-org-link-to-contact :which-key "link contact")
   "c/" '(helm-org-rifle :which-key "org-rifle")
   )
  ;; some functions for timing
  )
)

(use-package helm-bibtex :ensure t :after org)

(use-package org-ref
  :ensure t
  :after org
  :init
  (setq reftex-default-bibliography '("~/org/resources/bibliography/refs.bib"))
  ;; see org-ref for use of these variables
  (setq bibtex-completion-bibliography '("~/org/resources/bibliography/refs.bib"))
  (setq org-ref-default-citation-link "citep")
  :bind ("\C-c ]" . org-ref-cite-insert-helm)
  )

(defun org-build-agenda ()
  (interactive)
  (setq last-build-time (format-time-string "%S.%3N"))
  (org-agenda 0 " ")
  (setq after-build-time (format-time-string "%S.%3N"))
  (print last-build-time)
  (print after-build-time)
  )

;;; init-31-doc-org.el ends here
