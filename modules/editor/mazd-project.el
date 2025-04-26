;;; mazd-project.el --- Project -*- lexical-binding: t; -*-

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

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  )


(use-package consult-projectile
  :demand t
  :after (:all projectile consult embark)
  :config
  (defvar-keymap embark-consult-projectile-project-map
    :doc "Keymap to use for the projectile menu"
    :parent embark-general-map
    "g" #'magit-status)
  (add-to-list 'embark-keymap-alist '(consult-projectile-project embark-consult-projectile-project-map))
  (autoload 'magit-status "magit")
  :bind (:map projectile-command-map
              ("p" . consult-projectile-switch-project)))


(leader
  "p" '(:ignore t :which-key "Projects"))

(leader
  "fg" 'consult-git-grep
  )
(leader
  "pa" 'projectile-add-known-project
  "pc" 'projectile-kill-buffers
  "pf" 'projectile-find-file
  "pF" 'consult-projectile-find-file
  "pb" 'projectile-switch-to-buffer
  "pd" 'projectile-find-dir
  "pp" 'projectile-switch-project
  )

(provide 'mazd-project)
;;; mazd-project.el ends here
