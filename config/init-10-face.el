;;; init-10-face.el --- Customize the look of emacs

;; Copyright (C) 2016 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 20 Aug 2015

;; Keywords: configuration, zenburn
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:
;; Zenburn theme; default font is set to 'monaco'

;;; Code:
(require 'use-package)

;; Splash Screen to Org-mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'markdown-mode)

;; == Load Custom Theme ==

;;; Zenburn
;;(use-package zenburn-theme
;;  :ensure t
;;  :config (load-theme 'zenburn t)
;;  )

;;; Solarized
(use-package color-theme :ensure t)
(use-package color-theme-solarized
  :ensure t
  :init (load-theme 'solarized t)
  )

;; I prefer using a smaller font size than the default (and 'Monaco')
(if (eq system-type 'darwin)
    (custom-set-faces '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "nil" :family "Monaco")))))
  (custom-set-faces '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "Ubuntu Mono"))))))

;; Set default fill column
(setq-default fill-column 80)

;; quiet, please! No dinging!
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Disable menu bars, etc.
(if window-system (scroll-bar-mode -1))
(tool-bar-mode -1)
(menu-bar-mode -1)

;; No Backup Files
(setq make-backup-files nil)

;; Diminish extraneous info in the modeline
(diminish 'abbrev-mode)
(defun sk/diminish-auto-revert ()
  "Diminishes the 'auto-revert-mode' in the mode line."
  (interactive)
  (diminish 'auto-revert-mode ""))
(add-hook 'auto-revert-mode-hook 'sk/diminish-auto-revert)

;; Customize the modeline
(use-package validate :ensure t)
(setq line-number-mode 1)
(setq column-number-mode 1)
(setq ns-use-srgb-colorspace nil)
(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-define-segment gjstein-buffer-position
    "a better buffer position display"
    (let ((buffer-position (format-mode-line "%p")))
      (if (string= buffer-position "Top") "top"
	(if (string= buffer-position "Bottom") "bot"
	  (if (string= buffer-position "All") "all"
	    "%p")))
      )
    )
  (spaceline-define-segment gjstein-version-control
    "Version control information."
    (when vc-mode
      (powerline-raw
       (s-trim (concat
		(let ((backend (symbol-name (vc-backend (buffer-file-name)))))
		  (substring vc-mode (+ (length backend) 2)))
		(when (buffer-file-name)
		  (pcase (vc-state (buffer-file-name))
		    (`up-to-date " ")
		    (`edited "*")
		    (`added "@")
		    (`unregistered "?")
		    (`removed "-")
		    (`needs-merge " Con")
		    (`needs-update " Upd")
		    (`ignored "!")
		    (_ " Unk"))))))))
  
  (spaceline-compile
   'gjstein
   ;; Left side of the mode line (all the important stuff)
   '(((buffer-modified buffer-size input-method) :face highlight-face)
     '(buffer-id remote-host major-mode)
     ((point-position line-column gjstein-buffer-position) :separator "|" )
     process
     (org-clock)
     ((flycheck-error flycheck-warning flycheck-info) :when active)
     ((which-function projectile-root (gjstein-version-control :when active)) :separator ":")
     )
   ;; Right segment (the unimportant stuff)
   '((minor-modes :separator spaceline-minor-modes-separator) :when active))
  
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-gjstein)))))

(use-package powerline
  :ensure t
  :after spaceline-config
  :config (validate-setq
	   powerline-height (truncate (* 1.0 (frame-char-height)))
	   powerline-default-separator 'utf-8))

;;; init-10-face.el ends here
