;;; init-20-nav-interface.el --- Customize emacs interface (mostly Helm)

;; Copyright (C) 2017 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 20 Aug 2015

;; Keywords: configuration, helm
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:
;; Mostly configures helm-mode, which is great for getting around
;; For more on Helm, see:
;;     http://tuhdo.github.io/helm-intro.html
;;     https://github.com/emacs-helm/helm/wiki

;;; Code:
(require 'use-package)

(setq evil-want-C-i-jump nil)
(setq ffap-machine-p-known 'reject)

;; == Evil Mode ==
(use-package undo-tree :ensure t :init (global-undo-tree-mode))

(use-package evil
  :ensure t
  :diminish undo-tree-mode
  :config
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  (define-key evil-ex-map "b " 'helm-mini)
  (define-key evil-ex-map "e " 'helm-find-files)
  (evil-set-undo-system 'undo-tree)

  ;; Resolve sentence jumping issue
  (setq sentence-end-double-space nil)

  ;; esc quits
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
	(setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  )


(use-package evil-collection
  ;; Use evil keybindings within magit
  :after evil
  :ensure t
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init)
  )

(use-package general
  :ensure t
  :after evil
  :init
  (defvar gjs-leader-key "<SPC>")

  (defun gjstein-org-agenda ()
    "Open my custom agenda"
    (interactive)
    (org-agenda 0 " "))
  (defun gjstein-org-weekly-agenda ()
    "Open my custom agenda"
    (interactive)
    (org-agenda 0 "L"))

  (defun gjstein-open-ledger ()
    "Open my ledger file"
    (interactive)
    (find-file "~/ledger/main.ledger")
    (evil-goto-line)
    )

  ;; Global general keybindings
  (general-evil-setup)
  (general-define-key
   :prefix gjs-leader-key
   :states '(normal motion)
   ;; Need to unbind prefix key first
   "" nil
   ;; I want these everywhere
   "a" '(gjstein-org-agenda :which-key "agenda")
   "A" '(gjstein-org-weekly-agenda :which-key "weekly agenda")
   "g" '(magit-status :which-key "git")
   ;; org-mode keys
   ;;"c" '(:ignore t :which-key "Org Keys")
   "q" '(org-capture :which-key "Capture")
   "l" '(gjstein-open-ledger :which-key "Ledger")
   gjs-leader-key '(helm-M-x :which-key "M-x")
   )
  (general-define-key
   :states '(motion)
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right
   )
  )

(use-package evil-surround
  :ensure t
  :after evil
  :init
  (global-evil-surround-mode 1)
  )

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :ensure t
  :after evil
  :init
  (evil-snipe-mode 1)
  (setq evil-snipe-scope 'buffer
	evil-snipe-repeat-scope 'buffer)
  (push 'magit-mode evil-snipe-disabled-modes)
  (push 'org-agenda-mode evil-snipe-disabled-modes)
  ; (add-hook 'magit-mode-hook 'turn-off-evil-snipe-mode)
  ;(add-hook 'org-agenda-mode-hook 'turn-off-evil-snipe-mode)
  )


;; == Helm Mode ==
(use-package helm
  :ensure t
  :diminish helm-mode
  :init

  ;; Changes the helm prefix key
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  ;; Supress warning
  (setq ad-redefinition-action 'accept)

  :config
  (require 'helm)
  (require 'helm-files)

  ;; Additional key bindings
  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key [escape] 'helm-keyboard-quit helm-map)
  (bind-key "C-l" (kbd "RET") helm-map)

  (setq helm-split-window-in-side-p           t
	helm-move-to-line-cycle-in-source     t
	helm-ff-search-library-in-sexp        t
	helm-scroll-amount                    8
	helm-M-x-fuzzy-match                  t
	helm-ff-file-name-history-use-recentf t)

  (if (string-equal system-type "darwin")
      ;; This requires the 'ggrep' command to be installed for OSX
      (setq helm-grep-default-command
	    "ggrep --color=always -d skip %e -n%cH -e %p %f"
	    helm-grep-default-recurse-command
	    "ggrep --color=always -d recurse %e -n%cH -e %p %f"))
  (if (string-equal system-type "gnu/linux")
      (setq helm-grep-default-command
	    "grep --color=always -d skip %e -n%cH -e %p %f"
	    helm-grep-default-recurse-command
	    "grep --color=always -d recurse %e -n%cH -e %p %f"))

  (helm-mode 1)

  (defun spacemacs//hide-cursor-in-helm-buffer ()
    "Hide the cursor in helm buffers."
    (with-helm-buffer
      (setq cursor-in-non-selected-windows nil)))
  (add-hook 'helm-after-initialize-hook 'spacemacs//hide-cursor-in-helm-buffer)
  
  :bind (("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x)
	 :map helm-map
	 ("C-i" . helm-execute-persistent-action)
	 ("C-z" . helm-select-action)
	 ("C-j" . helm-next-line)
	 ("C-k" . helm-previous-line)
	 ("C-h" . helm-next-source)
	 ("C-S-h" . describe-key)
	 ("C-e" . hydra-helm-menu/body)
	 :map helm-find-files-map
	 ("C-l" . helm-execute-persistent-action)
	 ("C-h" . helm-find-files-up-one-level)
	 :map helm-read-file-map
	 ("C-l" . helm-execute-persistent-action)
	 ("C-h" . helm-find-files-up-one-level)
	 )
  )

(use-package swiper :ensure t)
(use-package swiper-helm
  :ensure t
  :after helm
  :init
  (general-define-key
   :prefix gjs-leader-key
   :states '(normal motion)
   "/" 'swiper-helm)
  )
(general-define-key
 :keymaps 'swiper-map
 :states '(normal motion)
 [escape] 'keyboard-escape-quit
 )					;

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  (setq which-key-idle-delay 0.2)
  :diminish which-key-mode
  )

;;; init-20-nav-interface.el ends here
