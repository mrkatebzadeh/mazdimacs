;;; mazd//consult.el --- Consult -*- lexical-binding: t; -*-

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


(use-package consult
  :async (:priority high :packages (consolt))
  :ensure t
  :defer t
  :commands
  (consult-grep
   consult-find
   consult-outline
   consult-line
   consult-buffer
   )
  :config
  (setq consult-buffer-sources
	'(consult--source-hidden-buffer
	  consult--source-modified-buffer
	  consult--source-buffer
	  consult--source-recent-file
	  consult--source-file-register
	  consult--source-bookmark
	  ))

  (defun mazd//consult-grep (&optional dir initial)
    "Search in DIR using ripgrep, honoring .gitignore and excluding .git/."
    (interactive "P")
    (let* ((default-directory
            (or (and dir (read-directory-name "Search directory: " nil nil t))
                (consult--project-root)
                default-directory))
           (consult-ripgrep-args
            "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number -g !.git/"))
      (consult-ripgrep default-directory initial)))

  (defalias 'consult-grep #'mazd//consult-grep)
  )

(use-package consult-tramp
  :ensure nil
  :defer t
  :commands (consult-tramp))

(use-package consult-flyspell
  :defer t
  :ensure t
  :bind ("M-g s" . consult-flyspell))

(use-package consult-yasnippet
  :after (yasnippet consult)
  :defer t
  :ensure t
  :bind ("M-g y" . consult-yasnippet))

;;; Files
(leader
  "fK" 'consult-yank-kill-ring
  "fr" 'consult-recent-file
  "ft" 'consult-tramp
  "ff" 'find-file)
;;; Insert
(leader
  "is" 'consult-yasnippet)

(leader
  "lt" 'consult-flymake)

(provide 'mazd-consult)
;;; mazd//consult.el ends here
