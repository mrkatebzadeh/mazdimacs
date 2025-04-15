;;; mazd//theme.el --- Theme -*- lexical-binding: t; -*-

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

(use-package auto-dim-other-buffers
  :disabled t
  :ensure t
  :if (display-graphic-p)
  :config
  (auto-dim-other-buffers-mode t)
  )

(use-package anisochromatic
  :vc (:url "https://github.com/isomatter-labs/anisochromatic-emacs")
  :ensure nil
  :defer t
  )


(provide 'mazd-theme)
;;; mazd//theme.el ends here
