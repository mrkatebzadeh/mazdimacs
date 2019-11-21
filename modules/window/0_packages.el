;;; packages.el --- Window -*- lexical-binding: t; -*-

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

;;; ace-window
(use-package ace-window
  :ensure t
  :init
    (global-set-key [remap other-window] 'ace-window))

;;; imenu
(use-package imenu
  :ensure t)

;;; imenu-list
(use-package imenu-list
    :ensure t
    :init
      (setq imenu-list-focus-after-activation t
            imenu-list-auto-resize t))
;;; zoom
(use-package zoom
  :ensure t
  :init
					;  (zoom-mode t)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))))
;;; dark room
(use-package darkroom
  :ensure t)

;;; packages.el ends here
