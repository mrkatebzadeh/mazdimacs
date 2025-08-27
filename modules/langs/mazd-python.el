;;; mazd//python.el --- Python  -*- lexical-binding: t; -*-

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
(use-package python
  :ensure t
  :defer t
  :init
  (defun mazd//python-mode-setup ()
    "Configure indentation for Python."
    (setq tab-width 4
          python-indent-offset 4
          indent-tabs-mode nil)        ;; never insert tabs
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
    (add-hook 'before-save-hook #'mazd//untabify-buffer nil t))

  (defun mazd//untabify-buffer ()
    "Convert all tabs in the buffer to spaces."
    (interactive)
    (untabify (point-min) (point-max)))

  (add-hook 'python-mode-hook #'mazd//python-mode-setup)
  (add-hook 'python-ts-mode-hook #'mazd//python-mode-setup)
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode)))

(use-package lsp-pyright
  :defer t
  :ensure t
  :preface
  (defun lsp-pyright-format-buffer ()
    (interactive)
    (when (and (executable-find "yapf") buffer-file-name)
      (call-process "yapf" nil nil nil "-i" buffer-file-name)))
  :hook ((python-mode python-ts-mode) . (lambda ()
                                          (require 'lsp-pyright)
                                          (lsp))) ;; or (lsp-deferred)
  :init
  (when (executable-find "python3")
    (setq lsp-pyright-python-executable-cmd "python3")))

(use-package poetry
  :defer t
  :ensure t)

(defun mazd//run-python-file ()
  "Run the current Python file in a dedicated buffer."
  (interactive)
  (when buffer-file-name
    (let ((command (format "%s %s"
                           (or lsp-pyright-python-executable-cmd "python3")
                           (shell-quote-argument buffer-file-name))))
      (compilation-start command t
                         (lambda (_) "*Python Run*")))))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-r") #'mazd//run-python-file))
(with-eval-after-load 'python-ts-mode
  (define-key python-ts-mode-map (kbd "C-c C-r") #'mazd//run-python-file))

;;; bindings
(general-define-key
 :prefix "SPC k"
 :states '(normal visual motion)
 :keymaps '(python-ts-mode-map python-map)
 "R" 'run-python
 "r" 'mazd//run-python-file)



(provide 'mazd-python)
;;; mazd//python.el ends here
