;;; byte-recompile-config.el --- <TITLE> -*- lexical-binding: t; -*-

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


(add-to-list 'load-path (locate-user-emacs-file "modules/"))
(setq load-prefer-newer t
      gc-cons-threshold 402653184)
(require 'config-core)
(let ((mazd//inhibit-async-byte-recompile-config t))
  (require 'config-package))
(byte-recompile-config)

(provide 'byte-recompile-config)
;;; byte-recompile-config.el ends here
