;;; init-45-coding-ROS.el --- Bindings & Tools for ROS

;; Copyright (C) 2017 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 01 Sep 2015

;; Keywords: configuration, yaml, ROS
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:
;; The Robot Operating System (ROS) requires a number of different disambiguated
;; tools for emacs editing. This file has some of the hooks and modes for
;; working with the various files.


;;; Code:


;; == File types ==
;; Web-mode for .launch files (effectively xml)
(add-to-list 'auto-mode-alist '("\\.launch?\\'" . web-mode))

;; == YAML Mode ==
(use-package yaml-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  )


;;; init-45-coding-ROS.el ends here
