;;; mk-python.el --- Python  -*- lexical-binding: t; -*-

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

(use-package python
  :ensure t
  :defer t
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :config
  (add-hook 'python-ts-mode-hook #'mk-python-setup 91)

  (mk-python-update-highlights)
  )

(use-package jupyter
  :ensure t
  :defer t)

(when (string= mk-language-server "lsp")
  (use-package lsp-pyright
    :defer t
    :ensure t
    :preface
    (defun lsp-pyright-format-buffer ()
      (interactive)
      (when (and (executable-find "yapf") buffer-file-name)
	(call-process "yapf" nil nil nil "-i" buffer-file-name)))
    :hook (((python-mode python-ts-mode) . (lambda ()
                                             (require 'lsp-pyright)
                                             (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t))))
    :init (when (executable-find "python3")
            (setq lsp-pyright-python-executable-cmd "python3")))
  )

;;;###autoload
(defun mk-py-hl-filter-self (node)
  (not (equal (treesit-node-text node) "self")))

;;;###autoload
(defun mk-python-update-highlights ()
  (setq python--treesit-settings
        (append python--treesit-settings
                (treesit-font-lock-rules

                 ;; custom rules
                 :language 'python
                 :override 'nil
                 :feature 'custom
                 '((call function: (identifier) @mk-font-lock-function-call-face))

                 :language 'python
                 :override 't
                 :feature 'custom
                 '((call function: (attribute attribute: (identifier) @mk-font-lock-method-call-face)))

                 :language 'python
                 :override 't
                 :feature 'custom
                 '(((identifier) @mk-font-lock-global-var-face
                    (:match "^_?[A-Z][A-Z_0-9]*$" @mk-font-lock-global-var-face)))

                 :language 'python
                 :override 't
                 :feature 'custom
                 '((call function: (identifier) @mk-font-lock-constructor-face
			 (:match "^_?[A-Z]" @mk-font-lock-constructor-face))
                   (call function: (attribute attribute: (identifier) @mk-font-lock-constructor-face)
                         (:match "^_?[A-Z]" @mk-font-lock-constructor-face)))

                 :language 'python
                 :feature 'custom
                 '((keyword_argument name: (identifier) @mk-font-lock-argument-keyword-face))

                 :language 'python
                 :feature 'custom
                 '(((parameters (identifier) @mk-font-lock-parameter-face
				(:pred mk-py-hl-filter-self @mk-font-lock-parameter-face)))

                   (parameters (typed_parameter (identifier) @mk-font-lock-parameter-face))
                   (parameters (default_parameter name: (identifier) @mk-font-lock-parameter-face))
                   (parameters (typed_default_parameter name: (identifier) @mk-font-lock-parameter-face))

                   (parameters
                    (list_splat_pattern ; *args
                     (identifier) @mk-font-lock-parameter-face))
                   (parameters
                    (dictionary_splat_pattern ; **kwargs
                     (identifier) @mk-font-lock-parameter-face))

                   (lambda_parameters
                    (identifier) @mk-font-lock-parameter-face))

                 :language 'python
                 :feature 'custom
                 '((argument_list (identifier) @mk-font-lock-argument-face)
                   (argument_list
                    (list_splat         ; *args
                     (identifier) @mk-font-lock-argument-face))
                   (argument_list
                    (dictionary_splat   ; **kwargs
                     (identifier) @mk-font-lock-argument-face)))
                 :language 'python
                 :override 't
                 :feature 'custom
                 '((list_splat_pattern "*" @font-lock-misc-punctuation-face)
                   (list_splat "*" @font-lock-misc-punctuation-face)
                   (dictionary_splat_pattern "**" @font-lock-misc-punctuation-face)
                   (dictionary_splat "**" @font-lock-misc-punctuation-face))))))

;;;###autoload
(defun mk-python-setup-highlight ()
  (unless (member 'custom (nth 2 treesit-font-lock-feature-list))
    (push 'custom (nth 2 treesit-font-lock-feature-list))
    (push 'font-lock-misc-punctuation-face (nth 2 treesit-font-lock-feature-list)))

  (setopt treesit-font-lock-level 3))

;;;###autoload
(defun mk-python-setup ()
  (mk-python-setup-highlight))


(provide 'mk-python)
;;; mk-python.el ends here
