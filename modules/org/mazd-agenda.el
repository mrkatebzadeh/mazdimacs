;;; mazd-agenda.el --- Agenda -*- lexical-binding: t; -*-

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

(use-package org-agenda
  :defer t
  :ensure nil
  :init
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-window-setup 'current-window))

;; Add graphical view of agenda
(use-package org-timeline
  :ensure t
  :hook (org-agenda-finalize . org-timeline-insert-timeline))

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
        ))

;;;###autoload
(defun mazd//org-agenda ()
  "Setup and open org-agenda"
  (interactive)
  (mazd//org-agenda-setup)
  (org-agenda))

;;;###autoload
(defun mazd//open-agenda-note-file()
  (interactive)
  (find-file org-default-notes-file))

(leader
  "oa" 'mazd//org-agenda
  "on" 'mazd//open-agenda-note-file
  )

(local-leader  org-agenda-mode-map
	       :prefix "SPC A"
	       "ht" 'org-agenda-todo
	       "" '(:ignore t :which-key "Agenda")
	       "h" '(:ignore t :which-key "headings")
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


(provide 'mazd-agenda)
;;; mazd-agenda.el ends here
