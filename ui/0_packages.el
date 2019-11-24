;;; packages.el --- UI -*- lexical-binding: t; -*-

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

;;; theme
;; welcome screen
(use-package page-break-lines
  :ensure t)

(use-package dashboard
  :ensure t)

;; theme
(use-package challenger-deep-theme
  :ensure t)
					;  :init  (load-theme 'challenger-deep t))

;; dracula-theme
(use-package dracula-theme
  :ensure t
  :init (load-theme 'dracula t))
;; all-the-icons
(use-package all-the-icons
  :ensure t)

(use-package telephone-line
  :ensure t
  :init
  (telephone-line-mode 1))

;;; parentheses
;; rainbow mode
(use-package rainbow-delimiters
  :ensure t)

;; highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t)

;; xkcd
(use-package xkcd
  :ensure t
  :init
  (setq xkcd-cache-dir (concat mk-emacs-dir ".xkcd/"))
  (setq xkcd-cache-latest (concat xkcd-cache-dir "latest/")))

;;; packages.el ends here
