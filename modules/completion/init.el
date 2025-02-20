;;; completion.el --- completion -*- lexical-binding: t; -*-

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


(require 'mazd-corfu)
(require 'mazd-consult)
(require 'mazd-eglot)
(require 'mazd-lsp)
(require 'mazd-company)

(defun mazd//switch-lsp ()
  "Switch between Eglot and LSP mode."
  (interactive)
  (if (string= mazd//language-server "eglot")
      (progn
        (when (bound-and-true-p eglot-managed-mode)
          (eglot-shutdown-all))
        (setq mazd//language-server "lsp")
        (lsp)
        (message "Switched to LSP Mode"))
    (progn
      (when (bound-and-true-p lsp-mode)
        (lsp-disconnect))
      (setq mazd//language-server "eglot")
      (eglot-ensure)
      (message "Switched to Eglot"))))

(defun mazd//start-lsp ()
  "Start the currently selected LSP backend."
  (interactive)
  (if (string= mazd//language-server "eglot")
      (eglot-ensure)
    (lsp)))

(leader
  "lS" 'mazd//switch-lsp)

(provide 'completion)
;;; completion.el ends here
