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
  )

(use-package flycheck-rust
  :ensure t
  :defer t
  :init
  
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

  )

;;; init-46-coding-rust.el ends here
