;;; init-46-coding-rust.el --- Coding for rust-lang

;; Copyright (C) 2017 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 15 Apr 2016

;; Keywords: configuration, rust, rust-lang
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:
;; Tools for programming the rust language
;;

(use-package rust-mode
  :ensure t
  :defer t
  :init
  (general-define-key
   :states '(normal motion)
   :prefix gjs-leader-key
   "ct" 'cargo-process-test
   "cb" 'cargo-process-build
   "cr" 'cargo-process-run
   "ce" 'cargo-process-bench
   (kbd "c RET") 'cargo-process-fmt
   )
  )

(use-package cargo
  :ensure t
  :after rust-mode
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  )

(use-package flycheck-rust
  :ensure t
  :defer t
  :init
  
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

  )

;;; init-46-coding-rust.el ends here
