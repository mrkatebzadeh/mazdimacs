;;; mazd//calc.el --- Calc  -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'mazd-vars)
  (require 'mazd-key)
  (require 'mazd-core))

(use-package calc
  :commands calc
  :defer  t)

;;; calc
(with-eval-after-load 'calc
  (setq math-additional-units '((GiB "1024 * MiB" "Giga Byte")
				(MiB "1024 * KiB" "Mega Byte")
				(KiB "1024 * B" "Kilo Byte")
				(B nil "Byte")
				(Gib "1024 * Mib" "Giga Bit")
				(Mib "1024 * Kib" "Mega Bit")
				(Kib "1024 * b" "Kilo Bit")
				(b "B / 8" "Bit"))
	math-units-table nil))

(leader
  "ac" 'calc)

(provide 'mazd-calc)
;;; mazd//calc.el ends here


;; Local Variables:
;; eval: (add-hook 'after-save-hook (lambda () (mazd//require-config-module 'mazd-calc) (message "Byte compilation completed for %s" buffer-file-name) ) nil t)
;; End:
