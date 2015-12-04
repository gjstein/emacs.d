;;; init-41-coding-c-cpp.el --- Programming in C/C++

;; Copyright (C) 2015 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 20 Aug 2015

;; Keywords: configuration, GNU GLOBAL, gtags, irony, c/c++
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:
;; Tools for working with C/C++. This consists of:
;;   helm-gtags :: make helm play nice with gtags
;;   irony :: a clang-based completion engine (works with company)

;;; Code:

;; === General Configuration
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; === Project Navigation ===

;; == gtags && helm-gtags ==
(use-package helm-gtags
  :ensure t
  :defer t
  :init
  ;; Enable helm-gtags-mode
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  :config
  (bind-key "C-c g a" 'helm-gtags-tags-in-this-function helm-gtags-mode-map)
  (bind-key "C-j" 'helm-gtags-select helm-gtags-mode-map)
  (bind-key "M-." 'helm-gtags-dwim helm-gtags-mode-map)
  (bind-key "M-," 'helm-gtags-pop-stack helm-gtags-mode-map)
  (bind-key "C-c <" 'helm-gtags-previous-history helm-gtags-mode-map)
  (bind-key "C-c >" 'helm-gtags-next-history helm-gtags-mode-map)
  )


;; === Code Completion ===

;; == company-backends ==
(defun my-c-company-hook ()
  "Company backends in C/C++."
  (setq company-backends '(company-irony company-gtags))
  )
(add-hook 'c-mode-hook 'my-c-company-hook)
(add-hook 'c++-mode-hook 'my-c-company-hook)

;; == irony-mode ==
(use-package irony
  :ensure t
  :defer t
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
  (use-package flycheck-google-cpplint
    :ensure t
    :config
;    (flycheck-add-next-checker 'c/c++-gcc ;; change if you don't use 'gcc'
					;			       'c/c++-googlelint)
    (add-to-list 'flycheck-checkers 'c/c++-googlelint)
    (custom-set-variables
     '(flycheck-google-cpplint-verbose "3")
     '(flycheck-google-cpplint-filter "-whitespace,+whitespace/braces")
     '(flycheck-google-cpplint-linelength "120"))
    ;; This requires that google cpplint be installed
    ;; See: https://github.com/flycheck/flycheck-google-cpplint
    
    )
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
