;;; mazd//ledger.el --- LEDGER -*- lexical-binding: t; -*-

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

(use-package ledger-mode
  :defer t
  :mode "\\.ledger$"
  :config (setq ledger-clear-whole-transactions 1))

(use-package evil-ledger
  :defer t
  :hook (ledger-mode . evil-ledger-mode))


(use-package flycheck-ledger
  :defer t
  :init (add-hook 'ledger-mode-hook #'flycheck-mode))

(local-leader ledger-mode-map
	      "" '(:ignore t :which-key "Ledger Mode")
	      "d" 'ledger-delete-current-transaction
	      "a" 'ledger-add-transaction
	      "b" 'ledger-post-edit-amount
	      "c" 'ledger-toggle-current
	      "C" 'ledger-mode-clean-buffer
	      "l" 'ledger-display-ledger-stats
	      "p" 'ledger-display-balance-at-point
	      "q" 'ledger-post-align-xact
	      "r" 'ledger-reconcile
	      "R" 'ledger-report
	      "t" 'ledger-insert-effective-date)

(local-leader ledger-reconcile-mode-map
	      "" '(:ignore t :which-key "Ledger Reconcile Mode")
	      "," 'ledger-reconcile-toggle
	      "a" 'ledger-reconcile-add
	      "q" 'ledger-reconcile-quit
	      "t" 'ledger-reconcile-change-target
	      "RET" 'ledger-reconcile-finish)

(provide 'mazd-ledger)
;;; mazd//ledger.el ends here
