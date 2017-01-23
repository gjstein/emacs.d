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
  :init
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  :diminish visual-line-mode
  :diminish org-indent-mode
  :defer t
  :bind (("\C-c a" . org-agenda)
	 ("\C-c c" . org-capture))
  :config

  (defun sk/diminish-org-indent ()
    "Diminish org-indent-mode on the modeline"
    (interactive)
    (diminish 'org-indent-mode ""))
  (add-hook 'org-indent-mode-hook 'sk/diminish-org-indent)

  ;; Fix evil-auto-indent for org buffers.
  (defun gs-org-disable-evil-auto-indent nil
    "Disables evil's auto-indent for org."
    (setq evil-auto-indent nil)
    )
  (add-hook 'org-mode-hook #'gs-org-disable-evil-auto-indent)
  
  ;; Custom functions for emacs & org mode
  (load-file "~/.emacs.d/config/gs-org.el")
  (require 'org)

  ;; == Agenda ==
  (defvar org-agenda-window-setup)
  (setq org-agenda-window-setup 'current-window)
  
  ;; Run/highlight code using babel in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (octave . t)
     (C . t)
     ))
  ;; Syntax hilight in #+begin_src blocks
  (setq org-src-fontify-natively t)

  ;; Capture mode
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ;; Evil key configurations (agenda)
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (defvar org-agenda-mode-map)
  (evil-define-key 'normal org-agenda-mode-map
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
    (kbd "SPC") 'org-agenda-show-and-scroll-up
    )
  (evil-leader/set-key-for-mode 'org-agenda-mode
    "i" 'org-agenda-clock-in
    "o" 'org-agenda-clock-out
    "k" 'org-agenda-kill
    "/" 'org-agenda-filter-by-tag
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
  
  (evil-define-key 'normal org-mode-map
    (kbd "<M-return>") 'gs-org-meta-return
    (kbd "<C-return>") 'gs-org-insert-heading-respect-content
    )
  (evil-leader/set-key-for-mode 'org-mode
    "i" 'org-clock-in
    "o" 'org-clock-out
    )

  ;; some functions for timing
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
