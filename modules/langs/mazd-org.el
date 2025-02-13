;;; mazd//org.el --- Org -*- lexical-binding: t; -*-

;; Copyright (C) 2019  M.R. Siavash Katebzadeh

;; Author: M.R.Siavash Katebzadeh <mr.katebzadeh@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

(use-package org
  :ensure nil
  :defer t
  :mode ("\\.org$" . org-mode)
  :hook
  (
   (org-mode . mazd//org-babel-setup)
   (org-mode . mazd//org-beamer-setup)
   (org-mode . mazd//org-agenda-setup)
   )
  :init
  (setq org-startup-with-inline-images t)
  :config
  (require 'org-id)
  )
(use-package org-inline-pdf
  :ensure t
  :defer t
  :hook (org-mode . org-inline-pdf-mode))

(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory (concat org-directory "/roam"))
  (org-roam-db-location (concat org-roam-directory "/org-roam.db"))
  (org-roam-completion-everywhere t)
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory))
  (setq org-agenda-files
	(append org-agenda-files
		(file-expand-wildcards (concat org-roam-directory "/*.org"))))
  (org-roam-setup))

(use-package org-roam-ui
  :after org-roam
  :ensure t
  :defer t
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t)
  )

(when (string= mazd//completion "featured")
  (use-package helm-org
    :ensure t
    :after helm
    :defer t)
  )

(use-package org-cliplink
  :ensure t
  :defer t
  :commands org-cliplink-capture)

(use-package ox-reveal
  :ensure t
  :after org
  )

(use-package org-modern
  :ensure t
  :defer t
  :custom
  (org-modern-fold-stars
   '(
     ("⬢" . "⬡")
     (" ⦿" . " ⭘")
     ("  ✦" . "  ✧")
     ("    ➤" . "    ➢")
     ("     ☀" . "     ☼")))
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  )

(use-package htmlize
  :ensure t
  :defer t)

(use-package gnuplot
  :ensure t
  :defer t)

(use-package org-ref
  :ensure t
  :defer t
  :init
  (setq interleave-org-notes-dir-list `(,(concat org-directory "/ref/files")))
  (setq org-ref-bibliography-notes     (concat org-directory "/ref/notes.org")
        org-ref-default-bibliography   (list (concat org-directory "/ref/master.bib"))
        org-ref-pdf-directory          (concat org-directory "/ref/files/"))
  ;; (setq org-latex-pdf-process '("latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))
  (setq org-ref-open-pdf-function
	(lambda (fpath)
	  (start-process "zathura" "*helm-bibtex-zathura*" "/usr/bin/zathura" fpath)))
  (defun mazd//set-libraries (library)
    "Set paths according to the selected library."
    (cond
     ((equal candidate "Research")
      (setq org-ref-bibliography-notes     (concat org-directory "/ref/notes.org")
	    org-ref-default-bibliography   (list (concat org-directory "/ref/master.bib"))
	    org-ref-pdf-directory          (concat org-directory "/ref/files/")
	    bibtex-completion-bibliography (concat org-directory "/ref/master.bib")
	    bibtex-completion-library-path (concat org-directory "/ref/files")
	    bibtex-completion-notes-path   (concat org-directory "/ref/notes.org")
	    helm-bibtex-bibliography bibtex-completion-bibliography
	    helm-bibtex-library-path bibtex-completion-library-path))
     ((equal candidate "Ebooks")
      (setq org-ref-bibliography-notes     (concat org-directory "/ebooks/notes.org")
	    org-ref-default-bibliography   (list (concat org-directory "/ebooks/master.bib"))
	    org-ref-pdf-directory          (concat org-directory "/ebooks/files/")
	    bibtex-completion-bibliography (concat org-directory "/ebooks/master.bib")
	    bibtex-completion-library-path (concat org-directory "/ebooks/files")
	    bibtex-completion-notes-path   (concat org-directory "/ebooks/notes.org")
	    helm-bibtex-bibliography bibtex-completion-bibliography
	    helm-bibtex-library-path bibtex-completion-library-path))
     ((equal candidate "PDFs")
      (setq org-ref-bibliography-notes     (concat org-directory "/pdfs/notes.org")
	    org-ref-default-bibliography   (list (concat org-directory "/pdfs/master.bib"))
	    org-ref-pdf-directory          (concat org-directory "/pdfs/files/")
	    bibtex-completion-bibliography (concat org-directory "/pdfs/master.bib")
	    bibtex-completion-library-path (concat org-directory "/pdfs/files")
	    bibtex-completion-notes-path   (concat org-directory "/pdfs/notes.org")
	    helm-bibtex-bibliography bibtex-completion-bibliography
	    helm-bibtex-library-path bibtex-completion-library-path))
     (t (message "Invalid!"))))
  (setq mazd//helm-libraries-source
	'((name . "Select a library.")
	  (candidates . ("Research" "Ebooks" "PDFs"))
	  (action . (lambda (candidate)
		      (mazd//set-libraries candidate)))))
  :config
  (defun my-orcb-key ()
    "Replace the key in the entry, also change the pdf file name if it exites."
    (let ((key (funcall org-ref-clean-bibtex-key-function
			(bibtex-generate-autokey))))
      ;; first we delete the existing key
      (bibtex-beginning-of-entry)
      (re-search-forward bibtex-entry-maybe-empty-head)

      (setq old-key (match-string 2));;store old key

      (if (match-beginning bibtex-key-in-head)
	  (delete-region (match-beginning bibtex-key-in-head)
			 (match-end bibtex-key-in-head)))
      ;; check if the key is in the buffer
      (when (save-excursion
	      (bibtex-search-entry key))
	(save-excursion
	  (bibtex-search-entry key)
	  (bibtex-copy-entry-as-kill)
	  (switch-to-buffer-other-window "*duplicate entry*")
	  (bibtex-yank))
	(setq key (bibtex-read-key "Duplicate Key found, edit: " key)))
      (insert key)
      (kill-new key)

      (save-excursion
	"update pdf names and notes items"
	;; rename the pdf after change the bib item key
	(my-update-pdf-names old-key key)
	;; renmae the notes item after change the bib item key
	(my-update-notes-item old-key key))

      ;; save the buffer
      (setq require-final-newline t)
      (save-buffer)))
  ;; define a function that update the pdf file names before change the key of a bib entry
  (defun my-update-pdf-names (old-key new-key)
    (let ((old-filename (concat org-ref-pdf-directory old-key ".pdf"))
	  (new-filename (concat org-ref-pdf-directory new-key ".pdf" )))
      (if (file-exists-p old-filename)
	  (rename-file old-filename new-filename))))
  ;; define a function that update the notes items before change the key of bib entry
  (defun my-update-notes-item (old-key new-key)
    "update a notes item of a old-key by a new-key in case the bib item is changed"

    (set-buffer (find-file-noselect org-ref-bibliography-notes))
    ;; move to the beginning of the buffer
    (goto-char (point-min))
    ;; find the string and replace it
    (let ((newcite new-key)
	  (regstr old-key))

      (while (re-search-forward regstr nil t)

	(delete-region (match-beginning 0)
		       (match-end 0))
	(insert newcite))

      ;; save the buffer
      (setq require-final-newline t)
      (save-buffer)
      (kill-buffer)))
  (add-hook 'org-ref-clean-bibtex-entry-hook 'my-orcb-key)
  )

;;; links
(defun mazd//org-clicky()
  "Open link at point if there is one, otherwise insert newline."
  (interactive)
  (if (org-in-regexp org-link-any-re)
      (org-open-at-point)
    (org-return)))

(add-hook 'org-mode-hook
          (lambda ()
            (evil-define-key 'normal org-mode-map (kbd "RET") 'mazd//org-clicky)))
(advice-add 'org-open-at-point :before #'org-mark-ring-push)
(setq org-link-frame-setup '((file . find-file)))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c b") 'org-mark-ring-goto)
  (define-key org-mode-map (kbd "C-c f") 'org-mark-ring-push))

;;; evil-org
(use-package evil-org
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme '(navigation insert textobjects)))))

(use-package org-agenda
  :defer t
  :ensure nil
  :init
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-window-setup 'current-window))

(use-package org-bullets
  :disabled t
  :ensure t
  :defer t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "◉" "○" "✸" "✿")))

(use-package org-contacts
  :ensure nil
  :defer t
  :after org
  :custom (org-contacts-files '((concat org-directory "/agenda/contacts.org"))))

(use-package org-faces
  :ensure nil
  :after org
  :defer t
  :custom
  (org-todo-keyword-faces
   '(("DONE" . (:foreground "cyan" :weight bold))
     ("SOMEDAY" . (:foreground "gray" :weight bold))
     ("TODO" . (:foreground "green" :weight bold))
     ("WAITING" . (:foreground "red" :weight bold)))))

(use-package org-crypt
  :ensure nil
  :after org
  :defer t
  :custom (org-crypt-key "mr.katebzadeh@gmail.com"))

(use-package org-journal
  :defer t
  :after org
  :preface
  (defun get-journal-file-yesterday ()
    "Gets filename for yesterday's journal entry."
    (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
           (daily-name (format-time-string "%Y%m%d" yesterday)))
      (expand-file-name (concat org-journal-dir daily-name))))

  (defun journal-file-yesterday ()
    "Creates and load a file based on yesterday's date."
    (interactive)
    (find-file (get-journal-file-yesterday)))
  :custom
  (org-journal-date-format "%e %b %Y (%A)")
  (org-journal-dir (format (concat org-directory "/journal/")
			   (format-time-string "%Y")))
  (org-journal-enable-encryption t)
  (org-journal-file-format "%Y%m%d")
  (org-journal-time-format ""))

(use-package org-gcal
  :disabled t
  :ensure t
  :defer t
  :config
  (load-library "~/Dropbox/org/keys/gcal.el.gpg"))

(use-package org-drill
  :defer t
  :ensure nil)

(defun mazd//org-drill ()
  "Load and run org-drill"
  (interactive)
  (require 'org-drill))

(use-package org-tvdb
  :disabled t
  :defer t
  :ensure nil ; remove this if available through melpa
  :config
  (load-library "~/Dropbox/org/keys/tvdb.el.gpg")
  :commands (org-tvdb-insert-todo-list
	     org-tvdb-add-season
	     org-tvdb-add-series
	     org-tvdb-mark-series-watched
	     org-tvdb-mark-season-watched
	     org-tvdb-update-series
	     org-tvdb-update-season))

(use-package ox-moderncv
  :defer t
  :ensure nil
  :load-path (lambda () (concat mazd//lisp-dir "/org-cv/")))

(use-package org-modern-indent
  :vc (:url "https://github.com/jdtsmith/org-modern-indent.git")
  :ensure nil
  :defer t
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

;; Add graphical view of agenda
(use-package org-timeline
  :ensure t
  :hook (org-agenda-finalize . org-timeline-insert-timeline))

(defun mazd//org-export()
  "Load required packages for exporting org file"
  (interactive)
  (require 'ox-moderncv)
  (require 'ox-reveal))

;;; config
(defun mazd//org-agenda-setup ()
  (setq org-agenda-files
        (append
         (file-expand-wildcards (concat org-directory "/agenda/*.org")))
        org-agenda-window-setup 'current-window
        org-deadline-warning-days 7
        org-agenda-span 'fortnight
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
        org-agenda-todo-ignore-deadlines 'all
        org-agenda-todo-ignore-scheduled 'all
        org-agenda-sorting-strategy '((agenda deadline-up priority-down)
                                      (todo priority-down category-keep)
                                      (tags priority-down category-keep)
                                      (search category-keep))
        org-default-notes-file (concat org-directory "/agenda/notes.org")
        org-capture-templates
        '(("t" "todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
          ("j" "Journal" entry (file+headline org-default-notes-file "Journal")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("n" "Note" entry (file+headline org-default-notes-file "Note")
           "* %?\nEntered on %U\n  %i")
          ("b" "Bookmark" entry (file+headline org-default-notes-file "Bookmark")
           "** %(org-cliplink-capture)\n:PROPERTIES:\n:TIMESTAMP: %t\n:END:%?\n" :empty-lines 1 :prepend t)
          ("r" "Research" entry (file+headline org-default-notes-file "Research")
           "** %(org-cliplink-capture)\n:PROPERTIES:\n:TIMESTAMP: %t\n:END:%?\n" :empty-lines 1 :prepend t)
          ("p" "Programming" entry (file+headline org-default-notes-file "Programming")
           "** %(org-cliplink-capture)\n:PROPERTIES:\n:TIMESTAMP: %t\n:END:%?\n" :empty-lines 1 :prepend t))))


(defun mazd//org-babel-setup ()
  "Setup org-babel languages and other configurations for org-mode."
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (setq org-confirm-babel-evaluate nil)
  (setq python-indent-offset 4)
  (setq org-edit-src-content-indentation 4)
  (require 'ob-python)
  (setq python-version-checked t)
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)
     (python . t)
     (shell . t)
     (ditaa . t)
     (plantuml . t)
     (dot . t)
     )))


(defun mazd//org-beamer-setup ()
  "Setup org-beamer"
  (unless (boundp 'org-export-latex-classes)
    (setq org-export-latex-classes nil))
  (add-to-list 'org-export-latex-classes
	       ;; beamer class, for presentations
	       '("beamer"
		 "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"

		 ("\\section{%s}" . "\\section*{%s}")

		 ("\\begin{frame}[fragile]\\frametitle{%s}"
		  "\\end{frame}"
		  "\\begin{frame}[fragile]\\frametitle{%s}"
		  "\\end{frame}")))

  ;; letter class, for formal letters

  (add-to-list 'org-export-latex-classes

	       '("letter"
		 "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"

		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


  (setq org-latex-create-formula-image-program 'imagemagick)
  (setq org-latex-packages-alist
	'(("" "color" t)
          ("" "minted" t)
          ("" "parskip" t)
          ("" "tikz" t))))

(with-eval-after-load 'org


  (setq mazd//secret-dir (concat org-directory "/keys/"))
  (setq org-todo-keywords '((sequence "TODO(t)"
				      "STARTED(s)"
				      "WAITING(w@/!)"
				      "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c@)")
			    (sequence "TOBUY"
				      "TOSHRINK"
				      "TOCUT"
				      "TOSEW" "|" "DONE(x)")
			    (sequence "TOWATCH"
				      "UNRELEASED"
				      "RELEASED" "|" "WATCHED(w)" "BREAK(b)")
			    (sequence "TODO"
				      "DOING"
				      "TESTING"
				      "ALMOST" "|" "DONE(x)")))


  (with-eval-after-load 'ox-reveal
    (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
	  org-reveal-mathjax t))

  (defun insert-file-as-org-table (filename)
    "Insert a file into the current buffer at point, and convert it to an org table."
    (interactive (list (ido-read-file-name "csv file: ")))
    (let* ((start (point))
	   (end (+ start (nth 1 (insert-file-contents filename)))))
      (org-table-convert-region start end)))

  (defun mazd//helm-ref ()
    "Prompt for switching libraries."
    (interactive)
    (require 'org-ref)
    (helm :sources '(mazd//helm-libraries-source)))
  )

(defun mazd//open-bib-file()
  (interactive)
  (find-file (car org-ref-default-bibliography)))

(defun mazd//open-note-file()
  (interactive)
  (find-file org-ref-bibliography-notes))

(defun mazd//open-agenda-note-file()
  (interactive)
  (find-file org-default-notes-file))

(defun mazd//eval-and-next-block ()
  "Evaluate the current Org mode source block and move to the next one."
  (interactive)
  (save-buffer)
  (org-babel-execute-src-block)
  (org-babel-next-src-block))

(defun mazd//org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (url (org-element-property :path link))
         (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

(defun mazd//org-link-open-eww ()
  "Open the Org mode link under the cursor directly in EWW."
  (interactive)
  (let* ((element (org-element-context))
         (type (org-element-property :type element))
         (path (org-element-property :path element))
         (link (concat type ":" path)))
    (if (and type path)
        (eww link)
      (message "No valid link found at point."))))


(defun mazd//org-code-execute ()
  "Execute the current code block, jump to the next code block, and center it in the buffer."
  (interactive)
  (org-babel-execute-maybe)
  (org-babel-next-src-block)
  (recenter))

;;; bindings
(leader
  "oa" 'org-agenda
  "oe" 'mazd//org-export
  "oo" 'org-mode
  "oc" 'org-capture
  "on" 'mazd//open-agenda-note-file
  "oR" '(:ignore t :which-key "org-ref")
  "oRn" 'mazd//open-note-file
  "oRo" 'mazd//open-bib-file
  "or" '(:ignore t :which-key "roam")
  "ort" 'org-roam-buffer-toggle
  "orf" 'org-roam-node-find
  "ori" 'org-roam-node-insert
  "oru" 'org-roam-ui-mode
  "org" 'org-roam-graph
  "orc" 'org-roam-capture
  "orj" 'org-roam-dailies-capture-today
  "oj" '(:ignore t :which-key "org-journal")
  "ojt" 'org-journal-new-entry
  "ojy" 'journal-file-yesterday
  "ol" 'org-store-link
  )

(when (string= mazd//completion "featured")
  (leader
    "oRs" 'mazd//helm-ref
    "oRi" 'org-ref-helm-insert-cite-link
    "oRl" 'helm-bibtex
    "oRd" 'doi-utils-add-bibtex-entry-from-doi
    )
  )

(when (string= mazd//completion "light")
  (leader
    "oRs" 'mazd//helm-ref
    "oRl" 'consult-bibtex
    )
  )


(evil-define-key 'normal bibtex-mode-map
  (kbd "C-j") 'org-ref-bibtex-next-entry
  (kbd "C-k") 'org-ref-bibtex-previous-entry
  "gj" 'org-ref-bibtex-next-entry
  "gk" 'org-ref-bibtex-previous-entry)

(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'bibtex-mode-map
 ;; Navigation
 "j" 'org-ref-bibtex-next-entry
 "k" 'org-ref-bibtex-previous-entry

 ;; Open
 "b" 'org-ref-open-in-browser
 "n" 'org-ref-open-bibtex-notes
 "p" 'org-ref-open-bibtex-pdf

 ;; Misc
 "h" 'org-ref-bibtex-hydra/body
 "i" 'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
 "s" 'org-ref-sort-bibtex-entry

 ;; Lookup utilities
 "l" '(:ignore t :which-key "lookup")
 "la" 'arxiv-add-bibtex-entry
 "lA" 'arxiv-get-pdf-add-bibtex-entry
 "ld" 'doi-utils-add-bibtex-entry-from-doi
 "li" 'isbn-to-bibtex
 "lp" 'pubmed-insert-bibtex-from-pmid)

(general-define-key
 :prefix "SPC k"
 :states '(normal visual motion)
 :keymaps 'org-mode-map
 ;; Source blocks / org-babel
 "b" '(:ignore t :which-key "blocks")
 "bp" 'org-babel-previous-src-block
 "bn"     'org-babel-next-src-block
 "be"     'mazd//org-code-execute
 "bE"     'org-babel-execute-maybe
 "bo"     'org-babel-open-src-block-result
 "bv"     'org-babel-expand-src-block
 "bu"     'org-babel-goto-src-block-head
 "bg"     'org-babel-goto-named-src-block
 "br"     'org-babel-goto-named-result
 "bb"     'org-babel-execute-buffer
 "bs"     'org-babel-execute-subtree
 "bd"     'org-babel-demarcate-block
 "bt"     'org-babel-tangle
 "bf"     'org-babel-tangle-file
 "bc"     'org-babel-check-src-block
 "bj"     'org-babel-insert-header-arg
 "bl"     'org-babel-load-in-session
 "bi"     'org-babel-lob-ingest
 "bI"     'org-babel-view-src-block-info
 "bz"     'org-babel-switch-to-session
 "bZ"     'org-babel-switch-to-session-with-code
 "ba"     'org-babel-sha1-hash
 "bx"     'org-babel-do-key-sequence-in-edit-buffer

 "c" 'org-todo
 "s" 'org-schedule
 "n" 'mazd//eval-and-next-block
 "C" '(:ignore t :which-key "org-crypt")
 "Ce" 'org-encrypt-entry
 "CE" 'org-encrypt-entries
 "Cd" 'org-decrypt-entry
 "CD" 'org-decrypt-entries
 "'" 'org-edit-special
 "d" 'org-deadline
 "D" 'org-insert-drawer
 "e" '(:ignore t :which-key "export")
 "ee" 'org-export-dispatch
 "f" 'org-set-effort
 "P" 'org-set-property
 ":" 'org-set-tags

 ;; "b" 'org-tree-to-indirect-buffer
 "A" 'org-archive-subtree
 "l" '(:ignore t :which-key "link")
 "ly" 'mazd//org-link-copy
 "le" 'mazd//org-link-open-eww
 "lo" 'org-open-at-point
 "lb" 'org-mark-ring-goto
 "lf" 'org-mark-ring-push

 "T" 'org-show-todo-tree
 "." 'org-time-stamp
 "!" 'org-time-stamp-inactive

 ;; headings
 "h" '(:ignore t :which-key "headings")
 "hi" 'org-insert-heading-after-current
 "hI" 'org-insert-heading
 "hs" 'org-insert-subheading

 ;; More cycling options (timestamps, headlines, items, properties)
 "L" 'org-shiftright
 "H" 'org-shiftleft
 "J" 'org-shiftdown
 "K" 'org-shiftup

 ;; Change between TODO sets
 "C-S-l" 'org-shiftcontrolright
 "C-S-h" 'org-shiftcontrolleft
 "C-S-j" 'org-shiftcontroldown
 "C-S-k" 'org-shiftcontrolup

 ;; Subtree editing
 "S" '(:ignore t :which-key "subtree")
 "Sl" 'org-demote-subtree
 "Sh" 'org-promote-subtree
 "Sj" 'org-move-subtree-down
 "Sk" 'org-move-subtree-up

 ;; tables
 "T" '(:ignore t :which-key "table")
 "Ta" 'org-table-align
 "Tb" 'org-table-blank-field
 "Tc" 'org-table-convert
 "Td" '(:ignore t :which-key "delete")
 "Tdc" 'org-table-delete-column
 "Tdr" 'org-table-kill-row
 "Te" 'org-table-eval-formula
 "TE" 'org-table-export
 "Th" 'org-table-previous-field
 "TH" 'org-table-move-column-left
 "Ti" '(:ignore t :which-key "insert")
 "Tic" 'org-table-insert-column
 "Tih" 'org-table-insert-hline
 "TiH" 'org-table-hline-and-move
 "Tir" 'org-table-insert-row
 "TI" 'org-table-import
 "Tj" 'org-table-next-row
 "TJ" 'org-table-move-row-down
 "TK" 'org-table-move-row-up
 "Tl" 'org-table-next-field
 "TL" 'org-table-move-column-right
 "Tn" 'org-table-create
 "TN" 'org-table-create-with-table.el
 "Tr" 'org-table-recalculate
 "Ts" 'org-table-sort-lines
 "Tt" '(:ignore t :which-key "toggles")
 "Ttf" 'org-table-toggle-formula-debugger
 "Tto" 'org-table-toggle-coordinate-overlays
 "Tw" 'org-table-wrap-region

 "t" '(:ignore t :which-key "toggles")
 "tc" 'org-toggle-checkbox
 "te" 'org-toggle-pretty-entities
 "ti" 'org-toggle-inline-images
 "tn" 'org-num-mode
 "tl" 'org-toggle-link-display
 "tt" 'org-show-todo-tree
 "tT" 'org-todo
 "tV" 'space-doc-mode
 "tx" 'org-latex-preview
 )

(general-define-key
 :prefix "SPC K"
 :states '(normal visual motion emacs)
 :keymaps 'org-agenda-mode-map
 "h" '(:ignore t :which-key "headings")
 "ht" 'org-agenda-todo
 "hk" 'org-agenda-kill
 "hr" 'org-agenda-refile
 "hA" 'org-agenda-archive-default
 "hT" 'org-agenda-set-tags
 "hp" 'org-agenda-priority

 ;; Visit entry
 "SPC" 'org-agenda-show-and-scroll-up
 "<tab>" 'org-agenda-goto
 "TAB" 'org-agenda-goto
 "RET" 'org-agenda-switch-to
 "o"   'link-hint-open-link

 ;; Date
 "d" '(:ignore t :which-key "date")
 "ds" 'org-agenda-schedule
 "dd" 'org-agenda-deadline
 "dt" 'org-agenda-date-prompt
 "+" 'org-agenda-do-date-later
 "-" 'org-agenda-do-date-earlier

 ;; View
 "v" '(:ignore t :which-key "view")
 "vd" 'org-agenda-day-view
 "vw" 'org-agenda-week-view
 "vt" 'org-agenda-fortnight-view
 "vm" 'org-agenda-month-view
 "vy" 'org-agenda-year-view
 "vn" 'org-agenda-later
 "vp" 'org-agenda-earlier
 "vr" 'org-agenda-reset-view

 ;; Toggle mode
 "t" '(:ignore t :which-key "toggle")
 "tf" 'org-agenda-follow-mode
 "tl" 'org-agenda-log-mode
 "ta" 'org-agenda-archives-mode
 "tr" 'org-agenda-clockreport-mode
 "td" 'org-agenda-toggle-diary

 ;; Filter
 "f" '(:ignore t :which-key "filter")
 "ft" 'org-agenda-filter-by-tag
 "fr" 'org-agenda-filter-by-tag-refine
 "fc" 'org-agenda-filter-by-category
 "fh" 'org-agenda-filter-by-top-headline
 "fx" 'org-agenda-filter-by-regexp
 "fd" 'org-agenda-filter-remove-all

 ;; Clock
 "c" '(:ignore t :which-key "clock")
 "ci" 'org-agenda-clock-in
 "co" 'org-agenda-clock-out
 "ck" 'org-agenda-clock-cancel
 "cj" 'org-agenda-clock-goto

 ;; Other
 "q" 'org-agenda-quit
 "gr" 'org-agenda-redo
 "." 'org-agenda-goto-today
 "gd" 'org-agenda-goto-date)

(eval-after-load 'org-agenda
  '(progn
     (evil-set-initial-state 'org-agenda-mode 'normal)
     (evil-define-key 'normal org-agenda-mode-map
       (kbd "<RET>") 'org-agenda-switch-to
       (kbd "\t") 'org-agenda-goto

       "q" 'org-agenda-quit
       "r" 'org-agenda-redo
       "S" 'org-save-all-org-buffers
       "gj" 'org-agenda-goto-date
       "gJ" 'org-agenda-clock-goto
       "gm" 'org-agenda-bulk-mark
       "go" 'org-agenda-open-link
       "s" 'org-agenda-schedule
       "+" 'org-agenda-priority-up
       "," 'org-agenda-priority
       "-" 'org-agenda-priority-down
       "y" 'org-agenda-todo-yesterday
       "n" 'org-agenda-add-note
       "t" 'org-agenda-todo
       ":" 'org-agenda-set-tags
       ";" 'org-timer-set-timer
       "I" 'helm-org-task-file-headings
       "i" 'org-agenda-clock-in-avy
       "O" 'org-agenda-clock-out-avy
       "u" 'org-agenda-bulk-unmark
       "x" 'org-agenda-exit
       "j"  'org-agenda-next-line
       "k"  'org-agenda-previous-line
       "vt" 'org-agenda-toggle-time-grid
       "va" 'org-agenda-archives-mode
       "vw" 'org-agenda-week-view
       "vl" 'org-agenda-log-mode
       "vd" 'org-agenda-day-view
       "vc" 'org-agenda-show-clocking-issues
       "g/" 'org-agenda-filter-by-tag
       "o" 'delete-other-windows
       "gh" 'org-agenda-holiday
       "gv" 'org-agenda-view-mode-dispatch
       "f" 'org-agenda-later
       "b" 'org-agenda-earlier
       "c" 'helm-org-capture-templates
       "e" 'org-agenda-set-effort
       "n" nil  ; evil-search-next
       "{" 'org-agenda-manipulate-query-add-re
       "}" 'org-agenda-manipulate-query-subtract-re
       "A" 'org-agenda-toggle-archive-tag
       "." 'org-agenda-goto-today
       "0" 'evil-digit-argument-or-evil-beginning-of-line
       "<" 'org-agenda-filter-by-category
       ">" 'org-agenda-date-prompt
       "F" 'org-agenda-follow-mode
       "D" 'org-agenda-deadline
       "H" 'org-agenda-holidays
       "J" 'org-agenda-next-date-line
       "K" 'org-agenda-previous-date-line
       "L" 'org-agenda-recenter
       "P" 'org-agenda-show-priority
       "R" 'org-agenda-clockreport-mode
       "Z" 'org-agenda-sunrise-sunset
       "T" 'org-agenda-show-tags
       "X" 'org-agenda-clock-cancel
       "[" 'org-agenda-manipulate-query-add
       "g\\" 'org-agenda-filter-by-tag-refine
       "]" 'org-agenda-manipulate-query-subtract)))

(provide 'mazd-org)
;;; mazd//org.el ends here
