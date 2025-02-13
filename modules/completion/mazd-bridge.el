;;; mazd//bridge.el --- Bridge -*- lexical-binding: t; -*-

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

(when (string= mazd//language-server "bridge")

  (use-package lsp-bridge
    :ensure t
    :vc (:fetcher github :repo "manateelazycat/lsp-bridge" :no-update t)
    :init
    (setq lsp-bridge-enable-inlay-hint t)

    (custom-set-faces
     '(lsp-bridge-inlay-hint-face ((t (:height 0.9 :inherit shadow :slant italic)))))
    (global-lsp-bridge-mode))


  (leader
    "ld" 'lsp-bridge-find-def
    "lD" 'lsp-bridge-find-impl
    "lR" 'lsp-bridge-find-references
    "lr" 'lsp-bridge-rename
    "la" 'lsp-bridge-code-action-popup-menu
    "lf" 'lsp-bridge-code-format
    "lk" 'lsp-bridge-show-documentation)

  )
(provide 'mazd-bridge)
;;; mazd//bridge.el ends here
