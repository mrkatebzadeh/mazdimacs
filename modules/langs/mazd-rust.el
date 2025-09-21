;;; mazd//rust.el --- Rust  -*- lexical-binding: t; -*-

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

;; (use-package rust-mode
;;   :defer t
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;;   (setq rust-mode-treesitter-derive t)
;;   :config
;;   (add-hook 'rust-mode-hook #'lsp)
;;   )

(use-package cargo
  :ensure t
  :defer t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :after (flycheck rust)
  :defer t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rustic
  :ensure t
  :defer t
  :init


  (setq rustic-lsp-client 'lsp)
  :config
  (setq lsp-eldoc-hook nil)
  (setq lsp-signature-auto-activate nil)
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(use-package crates
  :vc (:url "https://github.com/mrkatebzadeh/crates.el.git")
  :ensure nil
  :defer t
  :init
  (defun enable-crates-mode-if-cargo-toml ()
    "Enable `crates-mode` only if the buffer is Cargo.toml."
    (when (and (buffer-file-name)
               (string-equal (file-name-nondirectory (buffer-file-name)) "Cargo.toml"))
      (require 'crates)))

  (add-hook 'find-file-hook #'enable-crates-mode-if-cargo-toml)
  :custom
  (crates-checkmark-symbol " ")
  (crates-warning-symbol " "))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)

    (when (string= mazd//language-server "lsp")
      (setq-local lsp-inlay-hint-enable t)
      )
    )

  (add-hook 'before-save-hook 'lsp-format-buffer nil t)


  )

;;; bindings
(general-define-key
 :prefix "SPC k"
 :states '(normal visual motion)
 :keymaps 'rust-mode-map
 "c" '(:ignore t :which-key "cargo")
 "ca" 'cargo-process-add
 "cb" 'cargo-process-build
 "cn" 'cargo-process-new
 "cr" 'cargo-process-run)

(provide 'mazd-rust)
;;; mazd//rust.el ends here
