

;; === GTAGS ===

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

;; == company-mode ==
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay              nil
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	company-backends                '((company-irony company-gtags))
	)
  :bind ("C-;" . company-complete-common)
  )

;; === Syntax Highlighting ===

;; == MATLAB ==
(use-package matlab-mode
  :ensure t
  :defer t
  :config (matlab-cedet-setup)
  )


;; === Tools === 

;; == magit == 
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  )

(use-package python-mode
  :ensure t
  :defer t
  :init

  ;; Force the window configuration to remain the same even when comands are run
  ;; Must manually open the buffer in another window  
  ;; (setq py-keep-windows-configuration 'force)

  )


;; === Style && Syntax Checking ===

;; == flycheck ==
(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

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
    (flycheck-add-next-checker 'c/c++-gcc ;; change if you don't use 'gcc'
			       'c/c++-googlelint 'append)
    (custom-set-variables
     '(flycheck-google-cpplint-verbose "3")
     '(flycheck-google-cpplint-filter "-whitespace,+whitespace/braces")
     '(flycheck-google-cpplint-linelength "120"))
    ;; This requires that google cpplint be installed
    ;; See: https://github.com/flycheck/flycheck-google-cpplint
    
    )
  )
