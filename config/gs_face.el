
;; == General Face Properties ==


; Splash Screen to Org-mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)


;; == Load Custom Theme ==
;(use-package color-theme-solarized
;  :ensure t
;  :init (setq color-themes '())
;  :config
;  (set-frame-parameter nil 'background-mode 'light)
;  (load-theme 'solarized t)
;  )

(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t)
  )


;; Set Default Theme
;;   <Color theme initialization code>
;(setq current-theme 'unknown)
;(load-theme 'solarized-dark t)
;(defun synchronize-theme ()
;  (setq hour 
;	(string-to-number 
;	 (substring (current-time-string) 11 13))) ;;closes (setq hour...
;  (if (member hour (number-sequence 6 17))
;      (setq now 'solarized-light)
;    (setq now 'solarized-dark )) ;; end of (if ...
;  (if (equal now current-theme)
;      nil
;    (progn (setq current-theme now)
;	   (load-theme current-theme t)) ) ) ;; end of (defun ...
;(synchronize-theme)
;(run-with-timer 0 3600 'synchronize-theme)

(custom-set-faces '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "nil" :family "Monaco")))))

;(set-face-attribute 'helm-selection nil 
;                    :background "purple"
;                    :foreground "black")

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
