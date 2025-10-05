;;; mazd-basic.el --- MAZD-BASIC -*- lexical-binding: t; -*-

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

(use-package s
  :defer t
  :ensure t)

(use-package f
  :defer t
  :ensure t)

(use-package restart-emacs
  :defer t
  :ensure t)

(use-package exec-path-from-shell
  :defer t
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  )

(use-package gcmh
  :ensure t
  :init
  (setq gcmh-verbose             t
	gcmh-lows-cons-threshold #x800000
	gcmh-high-cons-threshold (* 128 1024 1024)
	gcmh-idle-delay 20
	)

  :config
  (gcmh-mode))

(use-package transient
  :ensure t)

(provide 'mazd-basic)
;;; mazd-basic.el ends here
