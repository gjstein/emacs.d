;;; init-30-doc-org.el --- Code for initializing org-mode

;; Copyright (C) 2016 Gregory J Stein

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

  :defer t
  :bind (("\C-c a" . org-agenda)
	 ("\C-c c" . org-capture))
  :config
  ;; Custom functions for emacs & org mode
  ;; (load-file "~/.emacs.d/config/bh-org.el")
  (load-file "~/.emacs.d/config/gs-org.el")

  ;; Agenda
  (setq org-agenda-window-setup 'current-window)
  ;;(setq org-agenda-files (quote ("~/org" "~/rrg/gjstein_notebook")))

  (define-key global-map "\C-cl" 'org-store-link)
  
  ; Run/highlight code using babel in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (octave . t)
     (C . t)
     ))
  ;; Syntax hilight in #+begin_src blocks
  (setq org-src-fontify-natively t)
  
  )

;;; init-31-doc-org.el ends here
