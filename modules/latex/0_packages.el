;;; packages.el --- Latex -*- lexical-binding: t; -*-

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

;;; auctex
(use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (setq TeX-auto-save t
	TeX-parse-self t)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
	      (TeX-source-correlate-mode)
              (turn-on-reftex)
              (reftex-isearch-minor-mode)
	      (add-to-list 'TeX-view-program-selection
			   '(output-pdf "Zathura"))
	      (setq-default TeX-master nil)
              (setq reftex-plug-into-AUCTeX t
                    TeX-PDF-mode t
		    TeX-command-force ""
		    TeX-source-correlate-method 'synctex
		    TeX-source-correlate-start-server t
		    ))))

  ;; to use pdfview with auctex

;;; reftex
(use-package reftex
  :ensure t)

;;; pdf-tools
(use-package pdf-tools
  :ensure t
  :defer 10
  :mode ("\\.pdf\\'" . pdf-tools-install))

;;; helm-bibtex
  (use-package helm-bibtex
    :ensure t)
;;; auctex-latexmk
(use-package auctex-latexmk
  :ensure t
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;;; company-auctex
(use-package company-auctex
  :ensure t
  :after (auctex company))

;;; packages.el ends here
