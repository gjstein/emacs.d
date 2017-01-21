;;; init-43-coding-matlab.el --- Coding for matlab

;; Copyright (C) 2017 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 20 Aug 2015

;; Keywords: configuration, matlab
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:
;; Runs matlab mode, which applies to both matlab documents and the matlab shell

;; == MATLAB ==
(use-package matlab-mode
  :ensure t
  :defer t
  :config (matlab-cedet-setup)
  )

;;; init-43-coding-matlab.el ends here
