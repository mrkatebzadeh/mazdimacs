;;; mazd-roam.el --- Roam -*- lexical-binding: t; -*-

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

(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory (concat org-directory "/roam"))
  (org-roam-db-location (concat org-roam-directory "/org-roam.db"))
  (org-roam-completion-everywhere t)
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory))
  (setq org-agenda-files
	(append org-agenda-files
		(file-expand-wildcards (concat org-roam-directory "/*.org"))))
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam
  :ensure t
  :defer t
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t)
  )

(leader
  :prefix "SPC r"
  "" '(:ignore t :which-key "Roam")
  "t" 'org-roam-buffer-toggle
  "f" 'org-roam-node-find
  "i" 'org-roam-node-insert
  "u" 'org-roam-ui-mode
  "g" 'org-roam-graph
  "c" 'org-roam-capture
  "j" 'org-roam-dailies-capture-today
  )

(provide 'mazd-roam)
;;; mazd-roam.el ends here
