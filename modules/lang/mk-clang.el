;;; mk-clang.el --- Clang  -*- lexical-binding: t; -*-

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

(use-package srefactor
  :defer t
  :config
  (semantic-mode 1))

(use-package ccls
  :defer t
  :after projectile
  :preface
  (defun mk-load-ccls-functions ()
    (setq ccls-sem-highlight-method 'font-lock)
    ;; alternatively, (setq ccls-sem-highlight-method 'overlay)
    (setq ccls-extra-init-params '(:completion (:detailedLabel t)))
    ;; For rainbow semantic highlighting
    (ccls-use-default-rainbow-sem-highlight)

    (defun ccls/callee () (interactive) (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
    (defun ccls/caller () (interactive) (lsp-ui-peek-find-custom "$ccls/call"))
    (defun ccls/vars (kind) (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
    (defun ccls/base (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
    (defun ccls/derived (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
    (defun ccls/member (kind) (interactive) (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

    ;; References w/ Role::Role
    (defun ccls/references-read () (interactive)
	   (lsp-ui-peek-find-custom "textDocument/references"
				    (plist-put (lsp--text-document-position-params) :role 8)))

    ;; References w/ Role::Write
    (defun ccls/references-write ()
      (interactive)
      (lsp-ui-peek-find-custom "textDocument/references"
			       (plist-put (lsp--text-document-position-params) :role 16)))

    ;; References w/ Role::Dynamic bit (macro expansions)
    (defun ccls/references-macro () (interactive)
	   (lsp-ui-peek-find-custom "textDocument/references"
				    (plist-put (lsp--text-document-position-params) :role 64)))

    ;; References w/o Role::Call bit (e.g. where functions are taken addresses)
    (defun ccls/references-not-call () (interactive)
	   (lsp-ui-peek-find-custom "textDocument/references"
				    (plist-put (lsp--text-document-position-params) :excludeRole 32)))

    ;; ccls/vars ccls/base ccls/derived ccls/members have a parameter while others are interactive.
    ;; (ccls/base 1) direct bases
    ;; (ccls/derived 1) direct derived
    ;; (ccls/member 2) => 2 (Type) => nested classes / types in a namespace
    ;; (ccls/member 3) => 3 (Func) => member functions / functions in a namespace
    ;; (ccls/member 0) => member variables / variables in a namespace
    ;; (ccls/vars 1) => field
    ;; (ccls/vars 2) => local variable
    ;; (ccls/vars 3) => field or local variable. 3 = 1 | 2
    ;; (ccls/vars 4) => parameter

    ;; References whose filenames are under this project
    (lsp-ui-peek-find-references nil (list :folders (vector (projectile-project-root))))
    )
  (defun mk-ccls ()
    (require 'ccls)
    (require 'company-c-headers)
    (lsp)
    (mk-load-ccls-functions))
  :hook ((c-mode c-common-mode c++-mode objc-mode) . mk-ccls)
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
	   projectile-project-root-files-top-down-recurring))
  :config (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

(use-package google-c-style
  :hook ((c++-mode) . google-set-c-style))
					;  (c-mode-common . google-make-newline-indent))

(use-package cmake-mode
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :defer t
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(defun clang-format-buffer-smart ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
    (clang-format-buffer)))
(defun clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))
(use-package clang-format
  :defer t
  :hook ((c-mode cc-mode c-common-mode c++-mode) . clang-format-buffer-smart-on-save))

(use-package disaster
  :defer t
  :commands (disaster disaster-objdump))

(use-package makefile-executor
  :defer t
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))

(use-package company-c-headers
  :defer t
  :config
  (push 'company-c-headers company-backends))

;;; config

(setq-default c-default-style "linux")

(defvar cmake-project-dir nil)
(defvar cmake-build-dir nil)
(defvar cmake-build-command nil)
(defvar cmake-make-command nil)
(defvar cmake-bclean-command nil)
(defvar cmake-mclean-command nil)

(defun cmake-find-project ()
  "Finds the directory of the project for cmake."
  (setq cmake-project-dir (projectile-project-root))
  (setq cmake-build-dir (concat cmake-project-dir "build"))
  (setq cmake-make-command
	(concat "cd " cmake-build-dir " && make"))
  (setq cmake-build-command
	(concat "cd " cmake-build-dir " && cmake .. && cp compile_commands.json .."))
  (setq cmake-bclean-command
	(concat "cd " cmake-build-dir " && rm -rf *"))
  (setq cmake-mclean-command
	(concat "cd " cmake-build-dir " && make clean"))
  )

(defun cmake-build ()
  (interactive)
  (shell-command cmake-build-command))

(defun cmake-make ()
  (interactive)
  (shell-command cmake-make-command))

(defun cmake-build-clean ()
  (interactive)
  (shell-command cmake-bclean-command))

(defun cmake-make-clean ()
  (interactive)
  (shell-command cmake-mclean-command))
(defun cmake-objdump-disaster (file-name)
  (require 'disaster)
  (let* ((objdump-cmd (format "%s %s" disaster-objdump (shell-quote-argument file-name)))
	 (buf (set-buffer (generate-new-buffer objdump-cmd))))
    (shell-command objdump-cmd buf)
    (read-only-mode)
    (asm-mode)
    (disaster--shadow-non-assembly-code)
    (switch-to-buffer-other-window buf)))

(defun cmake-find-obj-files ()
  (interactive)
  (let* ((exec-files (seq-filter 'file-readable-p
                                 (directory-files-recursively
                                  cmake-build-dir ".+\.o[bj]?$")))
         (base-buffer-name (file-name-base (buffer-name)))
         (calc-dist (lambda (fn) (cons fn
                                       (string-distance
                                        base-buffer-name
                                        (file-name-base fn)))))
         (cdr-< (lambda (a b) (< (cdr a) (cdr b))))
         (distances (sort (mapcar calc-dist exec-files) cdr-<)))
    (mapcar 'car distances)))

(defun cmake-obj-files-source ()
  (interactive)
  (require 'seq)
  `((name . "Object file to objdump")
    (candidates . ,(cmake-find-obj-files))
    (action . (lambda (sel) (cmake-objdump-disaster sel)))))

(defun cmake-objdump ()
  (interactive)
  (helm :sources (cmake-obj-files-source)))

(add-hook 'c-mode-common-hook 'cmake-find-project)



;;; bindings

(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps '(c-mode-map c++-mode-map)
 "gc" 'ccls/callee
 "gC" 'ccls/caller
 "gm" 'ccls/member
 "fr" 'lsp-ui-peek-find-references
 "fd" 'lsp-ui-peek-find-declaration
 "fD" 'lsp-ui-peek-find-definitions
 "r"  'lsp-rename
 "F"  'clang-format-buffer
 "i"  'lsp-ui-imenu
 "d"  'cmake-objdump
 "m"  'cmake-make
 "b"  'cmake-build
 "M"  'cmake-make-clean
 "B"  'cmake-build-clean
 "R"  'lsp-restart-workspace
 "h"  'lsp-symbol-highlight
 "s"  'srefactor-refactor-at-point)


(provide 'mk-clang)
;;; mk-clang.el ends here
