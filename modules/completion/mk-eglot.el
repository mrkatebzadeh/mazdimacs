;;; mk-eglot.el --- Eglot -*- lexical-binding: t; -*-

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

;;


(when (string= mk-language-server "eglot")
  (use-package eglot
    :defer t
    :hook ((rust-mode nix-mode c-mode python-mode latex-mode) . eglot-ensure)
    :config (add-to-list 'eglot-server-programs
			 `(rust-mode . ("rust-analyzer" :initializationOptions
					( :procMacro (:enable t)
					  :cargo ( :buildScripts (:enable t)
                                                   :features "all")))))
    (custom-set-faces
     '(eglot-inlay-hint-face ((t (:height 0.9 :inherit shadow :slant italic)))))
    (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1)))

    )
  (leader
    "ld" 'xref-find-definitions
    "lD" 'xref-find-def
    "lR" 'xref-find-references
    "lr" 'eglot-rename
    "la" 'eglot-code-actions
    "lf" 'eglot-format
    "lk" 'eldoc)


  )
(provide 'mk-eglot)
;;; mk-eglot.el ends here
