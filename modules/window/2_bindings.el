;;; bindings.el --- Window -*- lexical-binding: t; -*-

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

;;; Windows
(general-define-key
 :prefix "SPC w"
 :states '(normal visual motion)
 :keymaps 'override
 "v" 'evil-window-vsplit
 "s" 'evil-window-split
 "d" 'evil-window-delete
 "h" 'evil-window-left
 "j" 'evil-window-down
 "k" 'evil-window-up
 "l" 'evil-window-right)

;;; Toggles
(general-define-key
 :prefix "SPC t"
 :states '(normal visual motion)
 :keymaps 'override
 "i" 'imenu-list
 "I" 'helm-imenu
 "z" 'zoom-mode)


;;; bindings.el ends here
