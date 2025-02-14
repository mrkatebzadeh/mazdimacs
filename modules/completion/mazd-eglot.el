;;; mazd//eglot.el --- Eglot -*- lexical-binding: t; -*-

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

(when (string= mazd//language-server "eglot")
  (use-package eglot
    :defer t
    :preface
    (defun mazd//eglot-eldoc ()
      (setq eldoc-documentation-strategy
            'eldoc-documentation-compose-eagerly))
    :hook ((eglot-managed-mode . mazd//eglot-eldoc))
    :init
    (add-hook 'zig-mode-hook  'eglot-ensure)
    (add-hook 'rust-mode-hook 'eglot-ensure)
    (add-hook 'nix-mode-hook 'eglot-ensure)
    (add-hook 'python-mode-hook 'eglot-ensure)
    (add-hook 'python-ts-mode-hook 'eglot-ensure)
    (add-hook 'latex-mode-hook 'eglot-ensure)
    (add-hook 'c-mode-hook 'eglot-ensure)
    (add-hook 'c++-mode-hook 'eglot-ensure)
    ;; (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1)))
    :config
    (add-to-list 'eglot-server-programs
		 `((rust-mode rust-ts-mode) . ("rust-analyzer" :initializationOptions
					       ( :procMacro (:enable t)
						 :cargo ( :buildScripts (:enable t)
							  :features "all")))))

    (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
    (add-to-list 'eglot-server-programs
		 `(python-mode
                   . ,(eglot-alternatives '(
					    ("pyright-langserver" "--stdio")
					    "pylsp"
                                            "jedi-language-server"
					    ))))
    (custom-set-faces
     '(eglot-inlay-hint-face ((t (:height 0.9 :inherit shadow :slant italic)))))
    )
  (use-package eglot-booster
    :disabled t
    :vc (:url "https://github.com/jdtsmith/eglot-booster")
    :ensure nil
    :after eglot
    :config	(eglot-booster-mode))

  (leader
    "ld" 'xref-find-definitions
    "lD" 'xref-find-def
    "lR" 'xref-find-references
    "lr" 'eglot-rename
    "la" 'eglot-code-actions
    "lf" 'eglot-format
    "lk" 'eldoc-box-help-at-point)
  )
(provide 'mazd-eglot)
;;; mazd//eglot.el ends here
