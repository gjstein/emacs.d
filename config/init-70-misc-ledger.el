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
  (use-package flycheck-ledger
    :ensure t
    )
  )


