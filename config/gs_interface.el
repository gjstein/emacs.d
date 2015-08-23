;; === Interface ===
;; == Helm Mode ==
(use-package helm
  :ensure t
  :init
  :diminish
  (require 'helm-config) ; Necessary for helm-mode

  ;; Changes the helm prefix key
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  ;; Supress warning
  (setq ad-redefinition-action 'accept)

  :config
  ;; Key bindings
  (bind-key "<tab>" 'helm-execute-persistent-action helm-map) ; rebind TAB to run persistant action
  (bind-key "C-i" 'helm-execute-persistend-action helm-map) ; make TAB work in terminal
  (bind-key "C-z" 'helm-select-action helm-map) ; list actions using C-z

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-M-x-fuzzy-match                  t ; optional fuzzy matching for helm-M-x
	helm-ff-file-name-history-use-recentf t)

  ;; Make grep work on osx (FIXME for Linux)
  ;; This requires the 'ggrep' command to be installed
  (setq helm-grep-default-command
	"ggrep --color=always -d skip %e -n%cH -e %p %f"
	helm-grep-default-recurse-command
	"ggrep --color=always -d recurse %e -n%cH -e %p %f")

  ;; Don't use Helm for the following (reftex-citation lookup)
  (eval-after-load 'helm-mode
    '(add-to-list 'helm-completing-read-handlers-alist '(reftex-citation . nil))
    )
  
  (helm-mode 1)
  :bind (("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x)
	 )
  )

