;;; packages.el --- Mail -*- lexical-binding: t; -*-

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

;;; mu4e general settings
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(use-package mu4e
  :ensure nil
  :defer t)

;;; mu4e-conversation
(use-package mu4e-conversation
  :ensure t
  :after mu4e)

;;; alert
(use-package mu4e-alert
  :ensure t)

;;; gnus-dired
(use-package gnus
  :ensure t)

(use-package mu4e-maildirs-extension
  :ensure t
  :after mu4e)

;;; helm-mu
(use-package helm-mu
  :ensure t)

;;; packages.el ends here
