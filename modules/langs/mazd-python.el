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

;;;###autoload
(defun mazd//py-hl-filter-self (node)
  (not (equal (treesit-node-text node) "self")))

;;;###autoload
(defun mazd//python-update-highlights ()
  (setq python--treesit-settings
        (append python--treesit-settings
                (treesit-font-lock-rules

                 ;; custom rules
                 :language 'python
                 :override 'nil
                 :feature 'custom
                 '((call function: (identifier) @mazd//font-lock-function-call-face))

                 :language 'python
                 :override 't
                 :feature 'custom
                 '((call function: (attribute attribute: (identifier) @mazd//font-lock-method-call-face)))

                 :language 'python
                 :override 't
                 :feature 'custom
                 '(((identifier) @mazd//font-lock-global-var-face
                    (:match "^_?[A-Z][A-Z_0-9]*$" @mazd//font-lock-global-var-face)))

                 :language 'python
                 :override 't
                 :feature 'custom
                 '((call function: (identifier) @mazd//font-lock-constructor-face
			 (:match "^_?[A-Z]" @mazd//font-lock-constructor-face))
                   (call function: (attribute attribute: (identifier) @mazd//font-lock-constructor-face)
                         (:match "^_?[A-Z]" @mazd//font-lock-constructor-face)))

                 :language 'python
                 :feature 'custom
                 '((keyword_argument name: (identifier) @mazd//font-lock-argument-keyword-face))

                 :language 'python
                 :feature 'custom
                 '(((parameters (identifier) @mazd//font-lock-parameter-face
				(:pred mazd//py-hl-filter-self @mazd//font-lock-parameter-face)))

                   (parameters (typed_parameter (identifier) @mazd//font-lock-parameter-face))
                   (parameters (default_parameter name: (identifier) @mazd//font-lock-parameter-face))
                   (parameters (typed_default_parameter name: (identifier) @mazd//font-lock-parameter-face))

                   (parameters
                    (list_splat_pattern ; *args
                     (identifier) @mazd//font-lock-parameter-face))
                   (parameters
                    (dictionary_splat_pattern ; **kwargs
                     (identifier) @mazd//font-lock-parameter-face))

                   (lambda_parameters
                    (identifier) @mazd//font-lock-parameter-face))

                 :language 'python
                 :feature 'custom
                 '((argument_list (identifier) @mazd//font-lock-argument-face)
                   (argument_list
                    (list_splat         ; *args
                     (identifier) @mazd//font-lock-argument-face))
                   (argument_list
                    (dictionary_splat   ; **kwargs
                     (identifier) @mazd//font-lock-argument-face)))
                 :language 'python
                 :override 't
                 :feature 'custom
                 '((list_splat_pattern "*" @font-lock-misc-punctuation-face)
                   (list_splat "*" @font-lock-misc-punctuation-face)
                   (dictionary_splat_pattern "**" @font-lock-misc-punctuation-face)
                   (dictionary_splat "**" @font-lock-misc-punctuation-face))))))

;;;###autoload
(defun mazd//python-setup-highlight ()
  (unless (member 'custom (nth 2 treesit-font-lock-feature-list))
    (push 'custom (nth 2 treesit-font-lock-feature-list))
    (push 'font-lock-misc-punctuation-face (nth 2 treesit-font-lock-feature-list)))

  (setopt treesit-font-lock-level 3))

;;;###autoload
(defun mazd//python-setup ()
  (mazd//python-setup-highlight))



(use-package python
  :ensure t
  :defer t
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :config
  (add-hook 'python-ts-mode-hook #'mazd//python-setup 91)

  (mazd//python-update-highlights)
  )

(when (string= mazd//language-server "lsp")
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


(provide 'mazd-python)
;;; mazd//python.el ends here
