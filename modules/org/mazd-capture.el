;;; mazd-capture.el --- Capture -*- lexical-binding: t; -*-

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

(use-package org-cliplink
  :ensure t
  :defer t
  :commands org-cliplink-capture)

(mazd//after org
  (setq
   org-capture-templates
   '(("t" "todo" entry (file+headline org-default-notes-file "Tasks")
      "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
     ("j" "Journal" entry (file+headline org-default-notes-file "Journal")
      "* %?\nEntered on %U\n  %i\n  %a")
     ("n" "Note" entry (file+headline org-default-notes-file "Note")
      "* %?\nEntered on %U\n  %i")
     ("m" "Meeting"
      entry  (file+olp+datetree (lambda () (concat org-directory "/agenda/meetings.org")))
      "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
      :tree-type week
      :clock-in t
      :clock-resume t
      :empty-lines 0)
     ("b" "Bookmark" entry (file+headline org-default-notes-file "Bookmark")
      "** %(org-cliplink-capture)\n:PROPERTIES:\n:TIMESTAMP: %t\n:END:%?\n" :empty-lines 1 :prepend t)
     ("r" "Research" entry (file+headline org-default-notes-file "Research")
      "** %(org-cliplink-capture)\n:PROPERTIES:\n:TIMESTAMP: %t\n:END:%?\n" :empty-lines 1 :prepend t)
     ("p" "Programming" entry (file+headline org-default-notes-file "Programming")
      "** %(org-cliplink-capture)\n:PROPERTIES:\n:TIMESTAMP: %t\n:END:%?\n" :empty-lines 1 :prepend t)
     ("c" "Code" entry (file+headline org-default-notes-file "Code")
      "* TODO [#B] %?\n:Created: %T\n%i\n%a\nProposed Solution: "
      :empty-lines 0)
     ("d" "Work Log Entry"
      entry (file+olp+datetree (lambda () (concat org-directory "/agenda/work-log.org")))

      "* %?"
      :empty-lines 0)
     )
   org-todo-keywords '((sequence "TODO(t)"
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
				 "ALMOST" "|" "DONE(x)"))
   )

  )
(provide 'mazd-capture)
;;; mazd-capture.el ends here
