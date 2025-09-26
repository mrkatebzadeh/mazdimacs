;;; mazd-ref.el --- Ref -*- lexical-binding: t; -*-

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

(use-package org-latex
  :after org
  :defer t
  :ensure nil
  :config

  (setq org-latex-packages-alist
        (quote (("" "color" t) ("" "minted" t) ("" "parskip" t)))
        org-latex-pdf-process
        (quote (
		"pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
		"bibtex $(basename %b)"
		"pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
		"pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")))
  (add-to-list 'org-latex-classes
               `("copernicus_discussions"
                 "\\documentclass{copernicus_discussions}
               [NO-DEFAULT-PACKAGES]
               [PACKAGES]
               [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" "\\newpage" "\\subsection*{%s}" "\\newpage")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
               )
  )

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
	    ))
     ((equal candidate "Ebooks")
      (setq org-ref-bibliography-notes     (concat org-directory "/ebooks/notes.org")
	    org-ref-default-bibliography   (list (concat org-directory "/ebooks/master.bib"))
	    org-ref-pdf-directory          (concat org-directory "/ebooks/files/")
	    bibtex-completion-bibliography (concat org-directory "/ebooks/master.bib")
	    bibtex-completion-library-path (concat org-directory "/ebooks/files")
	    bibtex-completion-notes-path   (concat org-directory "/ebooks/notes.org")
	    ))
     ((equal candidate "PDFs")
      (setq org-ref-bibliography-notes     (concat org-directory "/pdfs/notes.org")
	    org-ref-default-bibliography   (list (concat org-directory "/pdfs/master.bib"))
	    org-ref-pdf-directory          (concat org-directory "/pdfs/files/")
	    bibtex-completion-bibliography (concat org-directory "/pdfs/master.bib")
	    bibtex-completion-library-path (concat org-directory "/pdfs/files")
	    bibtex-completion-notes-path   (concat org-directory "/pdfs/notes.org")
	    ))
     (t (mazd//err "Invalid!"))))

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

;;;###autoload
(defun mazd//open-bib-file()
  (interactive)
  (find-file (car org-ref-default-bibliography)))

;;;###autoload
(defun mazd//open-note-file()
  (interactive)
  (find-file org-ref-bibliography-notes))


(leader
  "oR" '(:ignore t :which-key "Ref")
  "oRn" 'mazd//open-note-file
  "oRo" 'mazd//open-bib-file
  )

(leader
  "oRd" 'doi-utils-add-bibtex-entry-from-doi
  )

(leader
  "oRl" 'consult-bibtex
  )

(evil-define-key 'normal bibtex-mode-map
  (kbd "C-j") 'org-ref-bibtex-next-entry
  (kbd "C-k") 'org-ref-bibtex-previous-entry
  "gj" 'org-ref-bibtex-next-entry
  "gk" 'org-ref-bibtex-previous-entry)

(general-define-key
 :prefix "SPC k"
 :states '(normal visual motion)
 :keymaps 'bibtex-mode-map
 "" '(:ignore t :which-key "Ref")

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
 "l" '(:ignore t :which-key "Lookup")
 "la" 'arxiv-add-bibtex-entry
 "lA" 'arxiv-get-pdf-add-bibtex-entry
 "ld" 'doi-utils-add-bibtex-entry-from-doi
 "li" 'isbn-to-bibtex
 "lp" 'pubmed-insert-bibtex-from-pmid)

(provide 'mazd-ref)
;;; mazd-ref.el ends here
