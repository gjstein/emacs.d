;;; init-41-coding-c-cpp.el --- Programming in C/C++

;; Copyright (C) 2017 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 20 Aug 2015

;; Keywords: configuration, GNU GLOBAL, gtags, irony, c/c++
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:
;; Tools for working with C/C++.  This consists of:
;;   helm-gtags :: make helm play nice with gtags
;;   irony :: a clang-based completion engine (works with company)

;;; Code:

;; === General Configuration
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; === Project Navigation ===

;; == gtags && helm-gtags ==
;; (use-package helm-gtags
;;   :ensure t
;;   :defer t

;;   :init
;;   ;; Enable helm-gtags-mode
;;   (add-hook 'dired-mode-hook 'helm-gtags-mode)
;;   (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;;   (add-hook 'c-mode-hook 'helm-gtags-mode)
;;   (add-hook 'c++-mode-hook 'helm-gtags-mode)
;;   (add-hook 'asm-mode-hook 'helm-gtags-mode)
;;   :config
;;   (bind-key "C-c g a" 'helm-gtags-tags-in-this-function helm-gtags-mode-map)
;;   (bind-key "C-j" 'helm-gtags-select helm-gtags-mode-map)
;;   (bind-key "M-." 'helm-gtags-dwim helm-gtags-mode-map)
;;   (bind-key "M-," 'helm-gtags-pop-stack helm-gtags-mode-map)
;;   (bind-key "C-c <" 'helm-gtags-previous-history helm-gtags-mode-map)
;;   (bind-key "C-c >" 'helm-gtags-next-history helm-gtags-mode-map)
;;   )

;; === Code Completion ===

;; == company-backends ==
(defun my-c-company-hook ()
 "Company backends in C/C++."
 (set (make-local-variable 'company-backends)
      '(company-dabbrev-code company-irony company-gtags))
 )
(add-hook 'c-mode-hook 'my-c-company-hook)
(add-hook 'c++-mode-hook 'my-c-company-hook)

;; == irony-mode ==
(use-package irony
  ;; Ubuntu:
  ;;  sudo apt-get install g++-4.8 clang-3.5 libclang-3.5-dev
  ;;  M-x irony-install-server
 :ensure t
 :defer t
 :diminish irony-mode
 :init
 (add-hook 'c++-mode-hook 'irony-mode)
 (add-hook 'c-mode-hook 'irony-mode)
 (add-hook 'objc-mode-hook 'irony-mode)
 :config
 ;; replace the `completion-at-point' and `complete-symbol' bindings in
 ;; irony-mode's buffers by irony-mode's function
 (defun my-irony-mode-hook ()
   (define-key irony-mode-map [remap completion-at-point]
     'irony-completion-at-point-async)
   (define-key irony-mode-map [remap complete-symbol]
     'irony-completion-at-point-async))
 (add-hook 'irony-mode-hook 'my-irony-mode-hook)
 (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
 )

;; === Other ===



;; == google-c-style && cpplint ==
(use-package google-c-style
  :ensure t
  :defer t
  :init
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)

  ;; == google style guide ==
  (load "~/.emacs.d/external/flycheck-google-cpplint")
  (add-to-list 'flycheck-checkers 'c/c++-googlelint)
  (custom-set-variables
   '(flycheck-c/c++-googlelint-executable "~/.emacs.d/scripts/cpplint.py")
   '(flycheck-google-cpplint-verbose "3")
   '(flycheck-google-cpplint-filter "-whitespace,+whitespace/braces")
   '(flycheck-google-cpplint-linelength "120"))
  ;; This requires that google cpplint be installed
  ;; See: https://github.com/flycheck/flycheck-google-cpplint
  (flycheck-add-next-checker 'c/c++-cppcheck 'c/c++-googlelint)

  :config
  (defun check-compile-options ()
  (interactive)
  (irony-cdb-json--ensure-project-alist-loaded)
  (irony--aif (irony-cdb-json--locate-db)
      (progn
        (message "I: found compilation database: %s" it)
        (let ((db (irony-cdb-json--load-db it)))
          (irony--aif (irony-cdb-json--exact-flags db)
              (progn
                (message "I: found exact match: %s" it)
                it)
            (let ((dir-cdb (irony-cdb-json--compute-directory-cdb db)))
              (irony--aif (irony-cdb-json--guess-flags dir-cdb)
                  (message "I: found by guessing: %s" it)
                (message "E: guessing failed"))))))
    (message "E: failed to locate compilation database")))
  )


;; === CMake ===
(use-package cmake-mode
  :ensure t
  :defer t
  :init
  ; Add cmake listfile names to the mode list.
  (setq auto-mode-alist
	(append
	 '(("CMakeLists\\.txt\\'" . cmake-mode))
	 '(("\\.cmake\\'" . cmake-mode))
	 auto-mode-alist))
  )

;;; init-41-coding-c-cpp.el ends here
