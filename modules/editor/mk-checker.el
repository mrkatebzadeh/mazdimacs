;;; mk-checker.el --- Checker  -*- lexical-binding: t; -*-

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

(use-package flycheck
  :defer t)

(use-package flyspell
  :defer t
  :custom
  (flyspell-abbrev-p t)
  (flyspell-default-dictionary "en_US")
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil))

(use-package flyspell-correct-helm
  :after (flyspell helm)
  :defer t
  :init (setq flyspell-correct-interface #'flyspell-correct-helm))

(use-package langtool
  :defer t
  :delight
  :custom
  (langtool-default-language "en")
  (langtool-language-tool-jar "~/.local/apps/LanguageTool-4.5/languagetool-commandline.jar")
  (langtool-language-tool-server-jar "~/.local/apps/LanguageTool-4.5/languagetool-server.jar")
  (langtool-mother-tongue "ir"))

;;; bindings
(general-define-key
 :prefix "SPC t"
 :states '(normal visual motion)
 :keymaps 'override
 "s" 'flyspell-mode
 "c" 'global-flycheck-mode)


(provide 'mk-checker)
;;; mk-checker.el ends here
