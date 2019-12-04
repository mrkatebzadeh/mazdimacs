;;; packages.el --- Org -*- lexical-binding: t; -*-

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

;;; latest org
(use-package org
  :ensure org-plus-contrib
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

;;; org-bullets
(use-package org-bullets
  :ensure t)

;;; org-reveal
(use-package ox-reveal
  :ensure ox-reveal)

(use-package htmlize
:ensure t)

;;; gnuplot
(use-package gnuplot
  :ensure t)

;;; org-ref
(use-package org-ref
  :ensure t
  :defer 7
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

;;; org-bullets
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "►" "▸")))

;;; org-contacts
(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '((concat org-directory "/agenda/contacts.org"))))

;;; org-faces
(use-package org-faces
  :ensure nil
  :after org
  :custom
  (org-todo-keyword-faces
   '(("DONE" . (:foreground "cyan" :weight bold))
     ("SOMEDAY" . (:foreground "gray" :weight bold))
     ("TODO" . (:foreground "green" :weight bold))
     ("WAITING" . (:foreground "red" :weight bold)))))

;;; org-crypt
(use-package org-cryptd
  :ensure nil
  :after org
  :custom (org-crypt-key "3797D501BCA4213083024D46533892D5073A452C"))

;;; org-journal
(use-package org-journal
  :ensure t
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

;;; org-cal
(use-package org-gcal
  :ensure t
  :config
  (load-library "~/Dropbox/org/keys/gcal.el.gpg"))

;;; org-drill
(use-package org-drill
  :ensure nil
  :init
  (require 'org-drill))

;;; org-tvdb
(use-package org-tvdb
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
  :ensure nil
  :load-path (lambda () (concat mk-lisp-dir "/org-cv/"))
  :init (require 'ox-moderncv))

;;; packages.el ends here
