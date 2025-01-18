;;; mazd//config.el --- Config  -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'mazd-vars)
  (require 'mazd-key)
  (require 'mazd-core))

(use-package try
  :ensure t
  :defer t)

(defun mazd//init-file ()
  "Open init.el file."
  (interactive)
  (find-file (concat mazd//emacs-dir "init.el")))

;;; bindigs
(leader
  "cc" 'mazd//init-file
  "ct" 'try)


(provide 'mazd-config)
;;; mazd//config.el ends here

;; Local Variables:
;; eval: (add-hook 'after-save-hook (lambda () (mazd//require-config-module 'mazd-config) (message "Byte compilation completed for %s" buffer-file-name) ) nil t)
;; End:
