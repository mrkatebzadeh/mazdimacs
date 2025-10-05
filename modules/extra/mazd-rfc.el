;;; mazd//rfc.el --- RFC -*- lexical-binding: t; -*-

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

(use-package rfc-mode
  :defer t)

;;;###autoload
(defun mazd//rfc()
  (interactive)
  (require 'rfc-mode)
  (rfc-mode-browse))

(mazd//after rfc-mode
  (setq rfc-mode-directory (expand-file-name "~/Dropbox/rfcs/"))
  (setq rfc-mode-index-path (concat rfc-mode-directory "rfc-index.txt"))
  (defun rfc-update ()
    "Updates rfc files."
    (interactive)
    (mazd//log "Syncing RFCs..")
    (shell-command (concat "rsync -qq -avz --delete ftp.rfc-editor.org::rfcs-text-only " rfc-mode-directory "rfc-update"))
    (mazd//log "RFCs are updated.")))

(leader
  "aR" 'mazd//rfc)

(local-leader rfc-mode-map
	      "" '(:ignore t :which-key "RFC Mode")
	      "g" 'rfc-mode-browse
	      "l" 'rfc-mode-read
	      "u" 'rfc-update
	      "q" 'rfc-mode-quit
	      "n" 'rfc-mode-forward-page
	      "p" 'rfc-mode-backward-page)

(provide 'mazd-rfc)
;;; mazd//rfc.el ends here
