;;; mazd-remote.el --- Remote -*- lexical-binding: t; -*-

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

(use-package ssh-deploy
  :ensure t
  :defer-incrementally (ssh-deploy)
  :init
  (setq ssh-deploy-revision-folder (concat mazd//cache-dir "ssh-revisions/")
        ssh-deploy-on-explicit-save 1
        ssh-deploy-automatically-detect-remote-changes nil)
  (dolist (sym '((ssh-deploy-root-local . stringp)
                 (ssh-deploy-root-remote . stringp)
                 (ssh-deploy-script . functionp)
                 (ssh-deploy-on-explicit-save . booleanp)
                 (ssh-deploy-force-on-explicit-save . booleanp)
                 (ssh-deploy-async . booleanp)
                 (ssh-deploy-exclude-list . listp)))
    (put (car sym) 'safe-local-variable (cdr sym)))

  (defun upload-init-after-save-h ()
    (when (and (bound-and-true-p ssh-deploy-root-remote)
	       (integerp ssh-deploy-on-explicit-save)
	       (> ssh-deploy-on-explicit-save 0))
      (ssh-deploy-upload-handler ssh-deploy-force-on-explicit-save)))

  (defun upload-init-find-file-h ()
    (when (bound-and-true-p ssh-deploy-root-remote)
      (require 'ssh-deploy)
      (unless ssh-deploy-root-local
	(setq ssh-deploy-root-local (doom-project-root)))
      (when ssh-deploy-automatically-detect-remote-changes
	(ssh-deploy-remote-changes-handler))))

  :commands (ssh-deploy-upload-handler
             ssh-deploy-upload-handler-forced
             ssh-deploy-diff-handler
             ssh-deploy-browse-remote-handler
             ssh-deploy-remote-changes-handler)
  :hook ((after-save . upload-init-after-save-h)
         (find-file  . upload-init-find-file-h))
  :config
  (ssh-deploy-line-mode)
  )
(provide 'mazd-remote)
;;; mazd-remote.el ends here
