;; User Info
(setq user-full-name "Gregory Stein")
(setq user-mail-address "gregory.j.stein@gmail.com")

;; Install use-package if necessary
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; Set the path variable
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; === Face Customization ===
(load-file "~/.emacs.d/config/gs_face.el")

;; === Interface ===
(load-file "~/.emacs.d/config/gs_interface.el")

;; === Programming & Coding Functions ===
(load-file "~/.emacs.d/config/coding.el")

;; === org-mode ===
(load-file "~/.emacs.d/config/gs_org.el")

;; === LaTeX ===
(load-file "~/.emacs.d/config/gs_tex_new.el")
