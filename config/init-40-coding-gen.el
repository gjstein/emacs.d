;;; init-40-coding-gen.el --- Code for general programming

;; Copyright (C) 2017 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 20 Aug 2015

;; Keywords: configuration, company, magit, git, flycheck
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:
;; General tools for programming across languages.  This consists of:
;;   Company :: used for code completion
;;   Projectile :: used for searching projects
;;   Magit :: used for interfacing with git/github
;;   Flycheck :: code syntax/convention checking

;;; Code:

;; === Code Completion ===

;; == company-mode ==
(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode)
  (general-define-key
   :keymaps 'company-active-map
   "C-j" 'company-select-next
   "C-k" 'company-select-previous
   "C-l" 'company-complete-selection)

  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay              nil
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	)
  (define-key evil-insert-state-map (kbd "C-f") 'company-complete-common)
  )

;; === Tools ===

;; == YASnippet ==
(use-package yasnippet
  :ensure t
  :defer t
  :config (yas-global-mode t)
  )

;; == ws-butler ==
;; This cleans up any whitespace I have at the end of my lines.
(use-package ws-butler
  :ensure t
  :init
  (ws-butler-global-mode)
  :diminish ws-butler-mode
  )

;; == Projectile ==
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-global-mode 1)
  (use-package helm-projectile
    :ensure t
    :after helm
    :config
    (helm-projectile-on)
    (general-define-key
     :prefix gjs-leader-key
     :states '(normal motion)
     ;; Ensure (leader p) maps to the projectile bindings
     "p" '(:keymap projectile-command-map :which-key "Projectile")
     "s" '(helm-projectile-ag :which-key "projectile ag")
     "p/" '(helm-projectile-ag :which-key "projectile ag")
     )
    )
  )

;; == ag ==
;; Note that 'ag' (the silver searcher) needs to be installed.
;; Ubuntu: sudo apt-get install silversearcher-ag
;; OSX: brew install ag
(use-package ag
  :ensure t
  :defer t
  )
(use-package helm-ag
  :ensure t
  :defer t
  :after helm
  :config
  (general-define-key :keymaps 'helm-ag-map
		      "C-c C-e" 'helm-ag-edit)
  ;; (bind-key "C-c C-e" 'helm-ag-edit helm-ag-mode-map)
  )

;; == compile ==

;; https://emacs.stackexchange.com/questions/8135/why-does-compilation-buffer-show-control-characters
(use-package ansi-color
  :ensure t
  :defer t
  :config (progn
            (defun my/ansi-colorize-buffer ()
              (let ((buffer-read-only nil))
                (ansi-color-apply-on-region (point-min) (point-max))))
            (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)))


;; == evil + vimish-fold ==
(use-package evil-vimish-fold
  :ensure t
  :defer t
  :init
  (evil-vimish-fold-mode 1)
  :diminish evil-vimish-fold-mode
  )

;; == magit ==
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-diff-options (quote ("--word-diff")))
  (setq magit-diff-refine-hunk 'all)
  ;; Use evil keybindings within magit
  :config
  (use-package evil-magit
    :ensure t
    :config
    ;; Default commit editor opening in insert mode
    (add-hook 'with-editor-mode-hook 'evil-insert-state)
    (evil-define-key 'normal with-editor-mode-map
      (kbd "RET") 'with-editor-finish
      [escape] 'with-editor-cancel
      )
    (evil-define-key 'normal git-rebase-mode-map
      "l" 'git-rebase-show-commit
      )
    )
  )

;; == flycheck ==
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; check OS type
  (if (string-equal system-type "gnu/linux")
      (progn
	(custom-set-variables
	 '(flycheck-c/c++-clang-executable "clang-3.5")
	 )))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
  )

;; == OTHER LANGUAGES ==
(use-package swift-mode
  :ensure t
  :defer t
  )

(use-package haskell-mode
  :ensure t
  :defer t
  :init
  )

;;; init-40-coding-gen.el ends here
