;;; mazd-babel.el --- Babel -*- lexical-binding: t; -*-

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

(defun mazd//org-babel-setup ()
  "Setup org-babel languages and other configurations for org-mode."
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (setq org-confirm-babel-evaluate nil)
  (setq python-indent-offset 4)
  (setq org-edit-src-content-indentation 4)
  (require 'ob-python)
  (require 'ob-C)
  (require 'jupyter)

  (setq org-babel-default-header-args:jupyter
	'((:results . "both")
	  (:session . (lambda () (buffer-file-name)))
	  (:kernel . "python3")
	  (:pandoc . "t")
	  (:exports . "both")
	  (:cache .   "no")
	  (:noweb . "no")
	  (:hlines . "no")
	  (:tangle . "no")
	  (:eval . "never-export")))

  (add-to-list 'org-src-lang-modes '("jupyter" . python))

  (defalias 'org-babel-execute:ipython 'org-babel-execute:jupyter)
  (setq org-babel-default-header-args:ipython org-babel-default-header-args:jupyter)
  (add-to-list 'org-src-lang-modes '("ipython" . python))


  (setq python-version-checked t)
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt")
  (add-to-list 'org-structure-template-alist '("s" . "src"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)
     (python . t)
     (shell . t)
     (ditaa . t)
     (plantuml . t)
     (dot . t)
     (C . t)
     (jupyter . t)
     )))


(add-hook 'org-mode-hook #'mazd//org-babel-setup)

(defun mazd//org-code-execute ()
  "Execute the current code block, jump to the next code block, and center it in the buffer."
  (interactive)
  (org-babel-execute-maybe)
  (org-babel-next-src-block)
  (recenter))


(use-package jupyter
  :ensure t
  :defer t)

(general-define-key
 :prefix "SPC k"
 :states '(normal visual motion)
 :keymaps 'org-mode-map
 "b" '(:ignore t :which-key "Babel")
 "bp" 'org-babel-previous-src-block
 "bn"     'org-babel-next-src-block
 "be"     'org-edit-special
 "bE"     'org-babel-execute-maybe
 "bo"     'org-babel-open-src-block-result
 "bv"     'org-babel-expand-src-block
 "bu"     'org-babel-goto-src-block-head
 "bg"     'org-babel-goto-named-src-block
 "br"     'org-babel-goto-named-result
 "bb"     'org-babel-execute-buffer
 "bs"     'org-babel-execute-subtree
 "bd"     'org-babel-demarcate-block
 "bt"     'org-babel-tangle
 "bf"     'org-babel-tangle-file
 "bc"     'org-babel-check-src-block
 "bj"     'org-babel-insert-header-arg
 "bl"     'org-babel-load-in-session
 "bi"     'org-babel-lob-ingest
 "bI"     'org-babel-view-src-block-info
 "bz"     'org-babel-switch-to-session
 "bZ"     'org-babel-switch-to-session-with-code
 "ba"     'org-babel-sha1-hash
 "bx"     'mazd//org-code-execute
 )


(provide 'mazd-babel)
;;; mazd-babel.el ends here
