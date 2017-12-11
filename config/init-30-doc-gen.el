;;; init-30-doc-gen.el --- Code for general document editing

;; Copyright (C) 2017 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Maintainer: Gregory J Stein <gregory.j.stein@gmail.com>
;; Created: 20 Aug 2015

;; Keywords: configuration, markdown, tex, latex, auctex
;; Homepage: https://github.com/gjstein/emacs.d
;; License: GNU General Public License (see init.el for details)

;;; Commentary:
;; Installs auctex (for latex super-editing) and markdown mode.

;;; Code:

;; == Markdown ==
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  )

(use-package flyspell
  :defer t
  :diminish (flyspell-mode . " φ"))

;; == LaTex / AucTeX ==
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (fset 'tex-font-lock-suscript 'ignore)
  (setq font-latex-fontify-script nil)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

  (general-define-key
   :keymaps 'LaTeX-mode-map
   :prefix (concat gjs-leader-key "c")
   :states '(normal visual)
   "c" 'TeX-command-master
   "v" 'TeX-view
   "e" 'LaTeX-environment
   "s" 'LaTeX-section
   "m" 'TeX-insert-macro
   "=" 'reftex-toc
   )
  
  ;; Don't use Helm for the reftex-citation lookup
  (eval-after-load 'helm-mode
    '(add-to-list 'helm-completing-read-handlers-alist '(reftex-citation . nil))
    )
  
  )

;;; init-30-doc-gen.el ends here
