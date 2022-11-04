;;; ox-c2c.el --- Org exporter (uses Pandoc)

(eval-when-compile (require 'cl-lib))
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
    (message tmp-org-file)
    (insert (org-c2c-run-pandoc tmp-org-file '~/org/resources/pandoc/cachestocaches.lua))
    (kill-buffer tmp-org-buffer)
    )
  )


(defun gjs-redef-reftex-get-bib-names ()
  (defun reftex-get-bib-names (field entry)
    "Return a list with the author or editor names in ENTRY.
If FIELD is empty try \"editor\" field."
    (let ((names (reftex-get-bib-field field entry)))
      (if (equal "" names)
	  (setq names (reftex-get-bib-field "editor" entry)))
					; Replace newline characters
      (while (string-match "\n" names)
	(setq names (replace-match " " nil t names)))
					; Replace duplicate spaces
      (while (string-match "  " names)
	(setq names (replace-match " " nil t names)))
      (while (string-match "[{}]" names)
	(setq names (replace-match "" nil t names)))
      (while (string-match "\\band\\b[ \t]*" names)
	(setq names (replace-match "\n" nil t names)))
      (while (string-match "\\(.*\\), \\(.*\\)" names)
	(setq names (replace-match "\\2 \\1" nil nil names)))
					; (while (string-match "[\\.a-zA-Z\\-]+\\.[ \t]*\\|,.*\\|[{}]+" names)
					;(setq names (replace-match "" nil t names)))
      (while (string-match "^[ \t]+\\|[ \t]+$" names)
	(setq names (replace-match "" nil t names)))
      (while (string-match "[ \t][ \t]+" names)
	(setq names (replace-match " " nil t names)))
      (while (string-match "[ \n][ \n]+" names)
	(setq names (replace-match " " nil t names)))
      (split-string names "\n")))
  )

(defun org-c2c-run-pandoc (filename output-format &optional options)
  (let* ((args (list "-f" "org"
		     "-t" (symbol-name output-format)
                     options
                     filename))
         (command (concat "pandoc " (mapconcat 'identity args " "))))
    (shell-command-to-string command)))

(defun gjs-org-ref-get-bibtex-entry-html-no-key (key)
  "Return an html string for the bibliography entry corresponding to KEY."
  (let ((output))
    (setq output (org-ref-get-bibtex-entry-citation key))
    (setq output (gjs-org-ref-clean-unused-entry-html output))
    (format "<li id=\"%s\">%s</li>"
	    key output)))

(defun gjs-org-ref-clean-unused-entry-html (entry-html)
  "Return from the html string ENTRY-HTML a cleaner version"
  ;; remove any stray "\url" commands
  (setq entry-html (replace-regexp-in-string "\\\\url" "" entry-html))
  ;; unescape the &
  (setq entry-html (replace-regexp-in-string "\\\\&" "&" entry-html))
  ;; hack to remove unnecessary \it
  (setq entry-html (replace-regexp-in-string "\\\\it " "" entry-html))
  ;; hack to remove duplicate spaces
  (setq entry-html (replace-regexp-in-string "  " " " entry-html))
  ;; hack to replace {} around text
  (setq entry-html (replace-regexp-in-string "{" "" entry-html))
  (setq entry-html (replace-regexp-in-string "}" "" entry-html))
  ;; get rid of empty parens
  (setq entry-html (replace-regexp-in-string "()" "" entry-html))
  ;; Remove empty volume, number field if empty
  (setq entry-html (replace-regexp-in-string "<b></b>," "" entry-html))
  ;; get rid of empty link and doi
  (setq entry-html (replace-regexp-in-string " <a href=\"\">link</a>\\." "" entry-html))
  ;; change double dash to single dash
  (setq entry-html (replace-regexp-in-string "--" "-" entry-html))
  (setq entry-html (replace-regexp-in-string " <a href=\"http://dx\\.doi\\.org/\">doi</a>\\." "" entry-html))
  entry-html)

(defun gjs-rm-cite-prefix (&optional key)
  (replace-regexp-in-string "cite.?:" "" key))

(defun gjs-c2c-bibliography-no-keys (&optional nocite-keys)
  (gjs-redef-reftex-get-bib-names)
  (let ((keys (append (org-ref-get-bibtex-keys t)
		      (mapcar 'gjs-rm-cite-prefix nocite-keys))))
    (when keys
      (concat "#+BEGIN_HTML\n<ul>\n"
	      (mapconcat (lambda (x) (gjs-org-ref-get-bibtex-entry-html-no-key x)) keys "\n")
	      "\n</ul>\n#+END_HTML"))))


(provide 'ox-c2c)
