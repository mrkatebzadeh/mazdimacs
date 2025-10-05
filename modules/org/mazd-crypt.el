;;; mazd-crypt.el --- Crypt -*- lexical-binding: t; -*-

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

(use-package org-crypt
  :ensure nil
  :after org
  :defer t
  :custom (org-crypt-key "mr.katebzadeh@gmail.com"))

(local-leader org-mode-map
	      "C" '(:ignore t :which-key "Crypt")
	      "Ce" 'org-encrypt-entry
	      "CE" 'org-encrypt-entries
	      "Cd" 'org-decrypt-entry
	      "CD" 'org-decrypt-entries
	      )

(provide 'mazd-crypt)
;;; mazd-crypt.el ends here
