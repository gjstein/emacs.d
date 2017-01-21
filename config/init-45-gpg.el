;;; init-45-gpg.el --- Encryption with gpg

;; Copyright (C) 2017 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 12 Jan 2016

;; Keywords: encryption, gpg
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:
;; Encrypts files that have a .gpg extension
;; Must have 'GnuPG' installed (from somewhere else)
;; Also, to get the pasword to not cache:
;;  - Put "default-cache-ttl 0" in file ~/.gnupg/gpg-agent.conf
;;  - Also "echo RELOADAGENT | gpg-connect-agent" can reboot 'gpg'
;; 

;;; Code:

;; Use bundled EasyPG
(require 'epa)
(epa-file-enable)
(setq epa-file-select-keys nil)


;;; init-45-gpg.el ends here
