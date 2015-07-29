
;; == General Face Properties ==

;; Splash Screen to Org-mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; == Load Custom Theme ==

;; Zenburn
(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t)
  )

;; I prefer using a smaller font size than the default (and 'Monaco')
(custom-set-faces '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "nil" :family "Monaco")))))

;; Set default fill column
(setq-default fill-column 80)

;; quiet, please! No dinging!
(setq visible-bell t)

;; Disable menu bars, etc.
(if window-system (scroll-bar-mode -1))
(tool-bar-mode -1)
(menu-bar-mode -1)

;; No Backup Files
(setq make-backup-files nil)
