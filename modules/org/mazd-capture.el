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

(with-eval-after-load 'org


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

  )
(provide 'mazd-capture)
;;; mazd-capture.el ends here
