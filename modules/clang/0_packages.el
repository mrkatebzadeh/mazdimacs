;;; packages.el --- Clang -*- lexical-binding: t; -*-

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

;;; srefactor
(use-package srefactor
  :ensure t
  :config
  (semantic-mode 1))

;;; ccls
(use-package ccls
  :ensure t
  :after (projectile yasnippet)
  :hook ((c-mode c-common-mode c++-mode objc-mode format-all-buffer-mode ) .
         (lambda () (require 'ccls) (lsp)))
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
	   projectile-project-root-files-top-down-recurring))
  :config (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

;;; google-c-style
(use-package google-c-style
  :hook ((c++-mode) . google-set-c-style))
					;  (c-mode-common . google-make-newline-indent))

;;; cmake-mode
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :ensure t
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

;;; clang-format
(defun clang-format-buffer-smart ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
    (clang-format-buffer)))
(defun clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))
(use-package clang-format
  :ensure t
  :hook (c-mode . clang-format-buffer-smart-on-save))

(use-package disaster
  :defer 5
  :commands (disaster)
  :hook ((c-mode c-common-mode c++-mode) . disaster))

;;; packages.el ends here
