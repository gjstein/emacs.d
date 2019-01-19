;;; init-44-coding-python.el --- Python configuration

;; Copyright (C) 2017 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 20 Aug 2015

;; Keywords: configuration, python
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:
;; Simply runs python-mode

;;; Code:


;; ensure:
;;; pip install jedi flake8 importmagic autopep8 yapf
(use-package elpy
  :ensure t
  :after python
  :config
  (elpy-enable)
  )

;;; init-44-coding-python.el ends here
