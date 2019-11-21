;;; bindings.el --- Clang -*- lexical-binding: t; -*-

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

;;; Clang-local
(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'c-mode-map
 "gc" 'ccls/callee
 "gC" 'ccls/caller
 "gm" 'ccls/member
 "fr" 'lsp-find-references
 "fd" 'lsp-find-declaration
 "fD" 'lsp-find-definition
 "r"  'lsp-rename
 "h"  'lsp-symbol-highlight
 )

;;; bindings.el ends here
