;;; mazd//git.el --- GIT  -*- lexical-binding: t; -*-

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

(use-package magit
  :ensure t
  :defer t
  :init
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package git-modes
  :ensure t)


(straight-use-package
 '(magit-pretty-graph :type git :host github :repo "georgek/magit-pretty-graph")
 )


(use-package magit-pretty-graph
  :ensure nil)

;;; config
(with-eval-after-load 'magit
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
;; (evil-magit-init))

;;; bindings
(with-eval-after-load 'magit
  (evil-collection-init 'magit))

(leader
  "gs" 'magit-status
  "gl" 'magit-pg-repo)

(leader
  "tg" 'git-gutter-mode)


(provide 'mazd-git)
;;; mazd//git.el ends here

;; Local Variables:
;; eval: (add-hook 'after-save-hook (lambda () (mazd//require-config-module 'mazd-git) (message "Byte compilation completed for %s" buffer-file-name) ) nil t)
;; End:
