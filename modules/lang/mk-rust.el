;;; mk-rust.el --- Rust  -*- lexical-binding: t; -*-

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

(use-package rust-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  :hook (rust-mode . lsp))

(use-package cargo
  :defer t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :after (flycheck rust)
  :defer t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;; bindings
(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'rust-mode-map
 "c" '(:ignore t :which-key "cargo")
 "ca" 'cargo-process-add
 "cb" 'cargo-process-build
 "cn" 'cargo-process-new
 "cr" 'cargo-process-run)


(provide 'mk-rust)
;;; mk-rust.el ends here
