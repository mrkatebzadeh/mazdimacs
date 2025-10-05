;;; mazd//latex.el --- LaTeX  -*- lexical-binding: t; -*-

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

(use-package lsp-latex
  ;; this uses texlab
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'bibtex-mode-hook 'lsp)
    )
  ;; (setq lsp-latex-build-on-save t)
  (setq tex-command "platex --synctex=1")

  ;; Setting for pdf-tools
  (setq lsp-latex-forward-search-executable "emacsclient")
  (setq lsp-latex-forward-search-args
	'("--eval"
          "(lsp-latex-forward-search-with-pdf-tools \"%f\" \"%p\" \"%l\")"))
  )

(use-package auctex
  :ensure t
  :defer t
  :hook ((latex-mode . flyspell-mode))
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (defun mazd//enable-auto-revert-for-pdf ()
    "Enable `global-auto-revert-mode` when opening a PDF file."
    (when (eq major-mode 'pdf-view-mode)
      (global-auto-revert-mode 1)))

  (add-hook 'pdf-view-mode-hook #'mazd//enable-auto-revert-for-pdf)
  (setq TeX-auto-save t
	TeX-parse-self t)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
	      (TeX-source-correlate-mode)
              (turn-on-reftex)
              (reftex-isearch-minor-mode)
	      ;; (add-to-list 'TeX-view-program-selection
	      ;; '(output-pdf "Zathura"))
	      ;; (add-to-list 'TeX-view-program-selection
	      ;; '(output-pdf "zathura"))
	      (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
		    TeX-source-correlate-start-server t)
	      (setq-default TeX-master nil)
              (setq reftex-plug-into-AUCTeX t
                    TeX-PDF-mode t
		    TeX-command-force ""
		    TeX-source-correlate-method 'synctex
		    TeX-source-correlate-start-server t
		    TeX-command-default "latex -synctex=1"
		    ))))

(use-package latex-preview-pane
  :ensure t
  :defer t)

(use-package reftex
  :ensure nil
  :defer t)

(use-package consult-bibtex
  :vc (:url "https://github.com/mohkale/consult-bibtex.git")
  :ensure nil
  :defer t
  :config
  (mazd//after embark
    (add-to-list 'embark-keymap-alist '(bibtex-completion . consult-bibtex-embark-map)))
  )

(use-package auctex-latexmk
  :ensure t
  :defer t
  :init
  (setq auctex-latexmazd//inherit-TeX-PDF-mode t))

(mazd//after auctex
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (add-hook 'after-save-hook 'TeX-command-master nil t)))
  (add-hook 'LaTeX-mode-hook 'pdf-tools-install)
  (add-hook 'LaTeX-mode-hook
	    '(lambda()
	       (define-key LaTeX-mode-map "\C-c\C-a" ; 'a' for ask
			   (lambda (arg) (interactive "P")
			     (let ((TeX-command-force nil))
			       (TeX-command-master arg)))))))

(mazd//after reftex
  (setq reftex-cite-prompt-optional-args t))


(mazd//after auctex-latexmk
  (auctex-latexmazd//setup))

;;;###autoload
(defun latex-compile ()
  (interactive)
  (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file))

(setq bibtex-completion-bibliography (concat org-directory "/ref/master.bib")
      bibtex-completion-library-path (concat org-directory "/ref/files")
      bibtex-completion-notes-path   (concat org-directory "/ref/notes.org")
      ;; using bibtex path reference to pdf file
      bibtex-completion-pdf-field "File"
      )

(local-leader LaTeX-mode-map
	      "b" 'latex-compile
	      "c" 'TeX-command-master
	      "\\" 'TeX-insert-macro
	      "-" 'TeX-recenter-output-buffer
	      "%" 'TeX-comment-or-uncomment-paragraph
	      ";" 'TeX-comment-or-uncomment-region
	      "a" 'TeX-command-run-all
	      "k" 'TeX-kill-job
	      "l" 'TeX-recenter-output-buffer
	      "i" '(:ignore t :which-key "insert")
	      "ii" 'LaTeX-insert-item
	      "im" 'TeX-insert-macro
	      "v" 'TeX-view
	      "hd" 'TeX-doc
	      "*" 'LaTeX-mark-section
	      "." 'LaTeX-mark-environment
	      "c" 'LaTeX-close-environment
	      "e" 'LaTeX-environment
	      "s" 'LaTeX-section
	      "f" '(:ignore t :which-key "fill")
	      "fe" 'LaTeX-fill-environment
	      "fp" 'LaTeX-fill-paragraph
	      "fr" 'LaTeX-fill-region
	      "fs" 'LaTeX-fill-section
	      "p" '(:ignore t :which-key "preview")
	      "pb" 'preview-buffer
	      "pc" 'preview-clearout
	      "pd" 'preview-document
	      "pe" 'preview-environment
	      "pf" 'preview-cache-preamble
	      "pp" 'preview-at-point
	      "pr" 'preview-region
	      "ps" 'preview-section
	      "r" '(:ignore t :which-key "reftex")
	      "rc" 'reftex-citation
	      "rg" 'reftex-grep-document
	      "ri" 'reftex-index-selection-or-word
	      "rI" 'reftex-display-index
	      "r TAB" 'reftex-index
	      "rl" 'reftex-label
	      "rp" 'reftex-index-phrase-selection-or-word
	      "rP" 'reftex-index-visit-phrases-buffer
	      "rr" 'reftex-reference
	      "rs" 'reftex-search-document
	      "rt" 'reftex-toc
	      "rT" 'reftex-toc-recenter
	      "rv" 'reftex-view-crossref)

(provide 'mazd-latex)
;;; mazd//latex.el ends here
