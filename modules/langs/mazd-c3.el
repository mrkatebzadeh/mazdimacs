;;; mazd-c3.el --- C3 -*- lexical-binding: t; -*-

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

(setq treesit-language-source-alist
      '((c3 "https://github.com/c3lang/tree-sitter-c3")))


(use-package c3-ts-mode
  :vc ( :url "https://github.com/c3lang/c3-ts-mode")
  :ensure nil
  :mode "\\.c3\\'"
  )

(provide 'mazd-c3)
;;; mazd-c3.el ends here
