;;; mazd//lsp.el --- LSP -*- lexical-binding: t; -*-

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

(use-package lsp-mode
  :async
  (:priority low :packages (lsp-mode-core lsp-protocol lsp-clients lsp-ui lsp-treemacs))
  :ensure t
  :defer t
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-keep-workspace-alive nil)
  (setq read-process-output-max (* 1024 1024))
  :custom
  (lsp-completion-provider :none)
  (lsp-prefer-flymake nil)
  (lsp-session-file (concat mazd//backup-dir "lsp-session-v1"))
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-enable-hover nil)
  (lsp-modeline-diagnostics-enable t)
  (lsp-idle-delay 0.6)
  (lsp-inlay-hint-enable t)
  (lsp-enable-symbol-highlighting t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  (lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
  :hook (
         (LaTeX-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
	 (c++-mode . lsp-defered)
	 ;; (rustic-mode . lsp-deferred)
	 (python-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . lsp-ui-mode)
	 (toml-mode . lsp-deferred)
	 )
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-diagnostics-provider :flycheck
        lsp-completion-provider  :none)
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-lens-enable                  nil
        lsp-modeline-code-actions-enable t
        lsp-modeline-code-action-fallback-icon "✦"
        lsp-signature-doc-lines 3)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("crates-lsp"))
    :major-modes '(toml-mode)
    :server-id 'crates-lsp
    :priority 1
    :initialized-fn (lambda (workspace)
                      ;; Put any init options here
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                         `(:crates ,(make-hash-table)))))))

  (mazd//after lsp-modeline
    (set-face-attribute 'lsp-modeline-code-actions-preferred-face nil
			:inherit font-lock-comment-face)
    (set-face-attribute 'lsp-modeline-code-actions-face nil
			:inherit font-lock-comment-face))
  )

(use-package lsp-ui
  :ensure t
  :demand t
  :after lsp-mode
  :commands (lsp-ui-peek-find-definitions
	     lsp-ui-peek-find-implementation
	     lsp-ui-peek-find-references)
  :config
  (setq lsp-prefer-flymake nil
	lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-ui-doc-max-height 15
	lsp-ui-doc-max-width 150
        lsp-ui-sideline-ignore-duplicate t
	lsp-ui-doc-enable t
	lsp-ui-doc-show-with-mouse t
	lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-update-mode "line"
	lsp-ui-sideline-show-hover nil)
  (setq lsp-headerline-breadcrumb-segments '(symbols))
  )

(use-package consult-lsp
  :ensure t
  :defer t
  :after lsp-mode
  )

(use-package dap-mode
  :ensure t
  :defer t
  :after lsp-mode
  :config
  (dap-mode -1)
  (dap-ui-mode -1)
  :bind
  (:map dap-mode-map
	(("<f12>" . dap-debug)
	 ("<f6>" . dap-breakpoint-condition)
	 ("<f8>" . dap-continue)
	 ("<f9>" . dap-next)
	 ("<M-f11>" . dap-step-in)
	 ("C-M-<f11>" . dap-step-out)
	 ("<f7>" . dap-breakpoint-toggle))))

(leader
  "lt" 'consult-lsp-diagnostics
  "ls" 'consult-lsp-symbols
  "lF" 'consult-lsp-file-symbols)

(leader
  "d" '(:ignore t :which-key "Debug")
  "dd" 'dap-debug
  "dB" 'dap-breakpoint-condition
  "dc" 'dap-continue
  "dn" 'dap-next
  "di" 'dap-step-in
  "do" 'dap-step-out
  "db" 'dap-breakpoint-toggle
  )

(leader
  "ld" 'lsp-ui-peek-find-definitions
  "lD" 'lsp-ui-peek-find-implementation
  "lR" 'lsp-ui-peek-find-references
  "lr" 'lsp-rename
  "la" 'lsp-execute-code-action
  "lf" 'format-all-buffer
  "lk" 'lsp-ui-doc-glance)


(provide 'mazd-lsp)
;;; mazd//lsp.el ends
