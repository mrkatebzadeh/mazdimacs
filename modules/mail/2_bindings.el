;;; bindings.el --- Mail -*- lexical-binding: t; -*-

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

;;; mu4e
(with-eval-after-load 'mu4e
  (evil-collection-init 'mu4e)
  (evil-define-key 'normal mu4e-headers-mode-map (kbd "/") 'helm-mu)
  (evil-define-key 'normal mu4e-headers-mode-map (kbd "C") 'helm-mu-contacts))
(general-define-key
 :prefix "SPC a"
 :states '(normal visual motion)
 :keymaps 'override
 "m" 'mu4e-init)

;;; bindings.el ends here
