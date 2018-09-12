;;; init-70-misc-ledger.el --- Enable ledger-mode

;; Copyright (C) 2017 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 20 Aug 2015

;; Keywords: configuration, ledger
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:n
;; Ledger mode

;;; Code:

(use-package ledger-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

  :config
  ; (use-package flycheck-ledger
  ;  :ensure t
  ;  )

  (general-define-key
   :keymaps 'ledger-mode-map
   :states '(normal motion)
   :prefix (concat gjs-leader-key "c")
   "r" 'ledger-report
   "R" 'ledger-report-redo
   "/" 'ledger-occur
   )

  (evil-set-initial-state 'ledger-report-mode 'motion)
  (general-define-key
   :keymaps 'ledger-report-mode-map
   :states '(normal motion)
   "e" 'ledger-report-edit-report
   "k" 'ledger-report-kill
   "q" 'ledger-report-quit
   "r" 'ledger-report-redo
   "s" 'ledger-report-save
   )
  )

(use-package flycheck-ledger
  :ensure t
  :after flycheck
  )

(use-package hledger-mode
  :ensure t
  :defer t
  :config
  ;; To open files with .journal extension in hledger-mode
  (add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

  ;; Provide the path to you journal file.
  ;; The default location is too opinionated.
  (setq hledger-jfile "~/ledger/main.ledger")


  ;; Auto-completion for account names
  ;; For company-mode users,
  (add-to-list 'company-backends 'hledger-company)
  )
