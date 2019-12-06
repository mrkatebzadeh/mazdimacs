;;; mk-org.el --- Org -*- lexical-binding: t; -*-

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

;;; Code:

(use-package org
  :ensure org-plus-contrib
  :defer t
  :pin org
  :mode ("\\.org$" . org-mode)
  :init
  (setq org-agenda-files
	(append
	 (file-expand-wildcards (concat org-directory "/agenda/*.org")))
	org-agenda-window-setup (quote current-window)
	org-deadline-warning-days 7
	org-agenda-span (quote fortnight)
	org-agenda-skip-scheduled-if-deadline-is-shown t
	org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
	org-agenda-todo-ignore-deadlines (quote all)
	org-agenda-todo-ignore-scheduled (quote all)
	org-agenda-sorting-strategy (quote
				     ((agenda deadline-up priority-down)
				      (todo priority-down category-keep)
				      (tags priority-down category-keep)
				      (search category-keep)))
	org-default-notes-file (concat org-directory "/agenda/notes.org")
	org-capture-templates
	'(("t" "todo" entry (file+headline org-default-notes-file "Tasks")
	   "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"))))

(use-package ox-reveal
  :defer t)

(use-package htmlize
  :defer t)

(use-package gnuplot
  :defer t)

(use-package org-ref
  :defer t
  :init
  (setq org-ref-bibliography-notes     (concat org-directory "/ref/notes.org")
        org-ref-default-bibliography   '(concat org-directory "/ref/master.bib")
        org-ref-pdf-directory          (concat org-directory "/ref/files/"))
  (setq org-latex-pdf-process '("latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))
  (setq interleave-org-notes-dir-list `(,(concat org-directory "/ref/files"))))

;;; evil-org
(use-package evil-org
  :ensure t
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme '(navigation insert textobjects))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "►" "▸")))

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

(use-package org-cryptd
  :ensure nil
  :after org
  :defer t
  :custom (org-crypt-key "3797D501BCA4213083024D46533892D5073A452C"))

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
  :defer t
  :config
  (load-library "~/Dropbox/org/keys/gcal.el.gpg"))

(use-package org-drill
  :defer t
  :ensure nil)

(defun mk-org-drill ()
  "Load and run org-drill"
  (interactive)
  (require 'org-drill))

(use-package org-tvdb
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
  :load-path (lambda () (concat mk-lisp-dir "/org-cv/")))

(defun mk-org-export()
  "Load required packages for exporting org file"
  (interactive)
  (require 'ox-moderncv)
  (require 'ox-reveal))

;;; config
(with-eval-after-load 'org
  (require 'org-id)
  (setq org-ref-open-pdf-function
	(lambda (fpath)
	  (start-process "zathura" "*helm-bibtex-zathura*" "/usr/bin/zathura" fpath)))

  (setq mk-secret-dir (concat org-directory "/keys/"))
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

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)))

  ;; org-beamer
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
	(quote (("" "color" t)
		("" "minted" t)
		("" "parskip" t)
		("" "tikz" t)))))

(with-eval-after-load 'org-bullets
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(with-eval-after-load 'ox-reveal
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
	org-reveal-mathjax t))

(defun insert-file-as-org-table (filename)
  "Insert a file into the current buffer at point, and convert it to an org table."
  (interactive (list (ido-read-file-name "csv file: ")))
  (let* ((start (point))
	 (end (+ start (nth 1 (insert-file-contents filename)))))
    (org-table-convert-region start end)))

(defun mk-helm-ref ()
  "Prompt for switching libraries."
  (interactive)
  (require 'org-ref)
  (helm :sources '(mk-helm-libraries-source)))


(with-eval-after-load 'org-ref
  (defun mk-set-libraries (library)
    "Set paths according to the selected library."
    (cond
     ((equal candidate "Research")
      (setq org-ref-bibliography-notes     (concat org-directory "/ref/notes.org")
	    org-ref-default-bibliography   '(concat org-directory "/ref/master.bib")
	    org-ref-pdf-directory          (concat org-directory "/ref/files/")
	    bibtex-completion-bibliography (concat org-directory "/ref/master.bib")
	    bibtex-completion-library-path (concat org-directory "/ref/files")
	    bibtex-completion-notes-path   (concat org-directory "/ref/notes.org")
	    helm-bibtex-bibliography bibtex-completion-bibliography
	    helm-bibtex-library-path bibtex-completion-library-path))
     ((equal candidate "Ebooks")
      (setq org-ref-bibliography-notes     (concat org-directory "/ebooks/notes.org")
	    org-ref-default-bibliography   '(concat org-directory "/ebooks/master.bib")
	    org-ref-pdf-directory          (concat org-directory "/ebooks/files/")
	    bibtex-completion-bibliography (concat org-directory "/ebooks/master.bib")
	    bibtex-completion-library-path (concat org-directory "/ebooks/files")
	    bibtex-completion-notes-path   (concat org-directory "/ebooks/notes.org")
	    helm-bibtex-bibliography bibtex-completion-bibliography
	    helm-bibtex-library-path bibtex-completion-library-path))
     ((equal candidate "PDFs")
      (setq org-ref-bibliography-notes     (concat org-directory "/pdfs/notes.org")
	    org-ref-default-bibliography   '(concat org-directory "/pdfs/master.bib")
	    org-ref-pdf-directory          (concat org-directory "/pdfs/files/")
	    bibtex-completion-bibliography (concat org-directory "/pdfs/master.bib")
	    bibtex-completion-library-path (concat org-directory "/pdfs/files")
	    bibtex-completion-notes-path   (concat org-directory "/pdfs/notes.org")
	    helm-bibtex-bibliography bibtex-completion-bibliography
	    helm-bibtex-library-path bibtex-completion-library-path))
     (t (message "Invalid!"))))
  (setq mk-helm-libraries-source
	'((name . "Select a library.")
	  (candidates . ("Research" "Ebooks" "PDFs"))
	  (action . (lambda (candidate)
		      (mk-set-libraries candidate)))))

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
  (add-hook 'org-ref-clean-bibtex-entry-hook 'my-orcb-key))

;;; bindings
(general-define-key
 :prefix "SPC o"
 :states '(normal visual motion)
 :keymaps 'override
 "a" 'org-agenda
 "e" 'mk-org-export
 "o" 'org-mode
 "c" 'org-capture
 "t" 'org-journal-new-entry
 "y" 'journal-file-yesterday
 "r" 'helm-bibtex
 "s" 'mk-helm-ref
 "l" 'org-store-link)


(provide 'mk-org)
;;; mk-org.el ends here
