;;; mazd-misc.el --- Misc -*- lexical-binding: t; -*-

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

(use-package org-inline-pdf
  :ensure t
  :defer t
  :hook (org-mode . org-inline-pdf-mode))

(use-package org-journal
  :defer t
  :after org
  :preface
  (defun get-journal-file-yesterday ()
    "Gets filename for yesterday's journal entry."
    (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
           (daily-name (format-time-string "%Y%m%d" yesterday)))
      (expand-file-name (concat org-journal-dir daily-name))))

;;;###autoload
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

(use-package org-cv
  :defer t
  :ensure (:host gitlab :repo "Titan-C/org-cv" :main "ox-moderncv.el"))

(leader
 "oj" '(:ignore t :which-key "org-journal")
 "ojt" 'org-journal-new-entry
 "ojy" 'journal-file-yesterday
 )

(provide 'mazd-misc)
;;; mazd-misc.el ends here
