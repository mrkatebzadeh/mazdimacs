;;; mk-lsp.el --- LSP -*- lexical-binding: t; -*-

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

;;; lsp-mode
(use-package lsp-mode
  :defer t
  :hook ((prog-mode) . lsp)
  :custom
  (lsp-prefer-flymake nil)
  (lsp-session-file (concat mk-backup-dir "lsp-session-v1"))
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
		    :major-modes '(python-mode)
		    :server-id 'pyls)))

(use-package lsp-ui
  :defer t
  :after lsp-mode)

(use-package company-lsp
  :defer t
  :after (company lsp-mode)
  :init
  (defvar company-lsp-enable-recompletion t)
  (defvar company-lsp-async t)
  :config
  (setq company-backends '(company-lsp company-yasnippet)))

(use-package dap-mode
  :defer t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))


(provide 'mk-lsp)
;;; mk-lsp.el ends here
