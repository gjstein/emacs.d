;;; ox-c2c.el --- Org exporter (uses Pandoc)

(eval-when-compile (require 'cl))
(require 'ox-md)

(org-export-define-derived-backend 'cachestocaches 'md
  :menu-entry
  '(?G "Export with C2C"
       ((?G "To buffer"
            (lambda (a s v b) (org-c2c-export-to-c2c a s v)))))
  )

(defun org-c2c-export-to-c2c (&optional async subtreep visible-only)
  "Convert org to my cachestocaches-flavored markdown via pandoc."
  (interactive)
  (message "MADE IT")
  (let ((tmp-org-buffer (org-org-export-as-org async subtreep visible-only))
	(tmp-org-file (make-temp-file "c2c_export")))
    (write-region nil nil tmp-org-file)
    (switch-to-buffer "*Org C2C Export*")
    (erase-buffer)
    (insert (org-c2c-run-pandoc tmp-org-file '~/org/resources/pandoc/cachestocaches.lua))
    (kill-buffer tmp-org-buffer)
    )
  )

(defun org-c2c-run-pandoc (filename output-format &optional options)
  (let* ((args (list "-f" "org"
		     "-t" (symbol-name output-format)
                     options
                     filename))
         (command (concat org-pandoc-command " " (mapconcat 'identity args " "))))
    (shell-command-to-string command)))

(provide 'ox-c2c)
