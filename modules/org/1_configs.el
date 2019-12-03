;;; configs.el --- Org -*- lexical-binding: t; -*-

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

;;; org
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

;;; org-bullets
(with-eval-after-load 'org-bullets
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;; ox-reveal
(with-eval-after-load 'ox-reveal
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
	org-reveal-mathjax t))

;;; insert csv as table
(defun insert-file-as-org-table (filename)
  "Insert a file into the current buffer at point, and convert it to an org table."
  (interactive (list (ido-read-file-name "csv file: ")))
  (let* ((start (point))
	 (end (+ start (nth 1 (insert-file-contents filename)))))
    (org-table-convert-region start end)))

;;; org-ref
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

;;; configs.el ends here
