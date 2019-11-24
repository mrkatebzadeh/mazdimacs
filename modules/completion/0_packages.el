;;; packages.el --- Completion -*- lexical-binding: t; -*-

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

;;; company mode
(use-package company
  :ensure t
  :bind
  (:map company-active-map
	("C-n" . company-select-next)
	("C-p" . company-select-previous)
	("<tab>" . company-complete-common-or-cycle)
	:map company-search-map
	("C-p" . company-select-previous)
	("C-n" . company-select-next))
  )

;;; company box
(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode)
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setq company-box-backends-colors nil)
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50)

  (defun company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
	(cond ((fboundp sym) 'Function)
	      ((featurep sym) 'Module)
	      ((facep sym) 'Color)
	      ((boundp sym) 'Variable)
	      ((symbolp sym) 'Text)
	      (t . nil)))))

  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-fileicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
	  `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
	    (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
	    (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
	    (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
	    (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
	    (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
	    (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
	    (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
	    (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
	    (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
	    (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
	    (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
	    (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
	    (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
	    (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
	    (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
	    (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
	    (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
	    (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
	    (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
	    (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
	    (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
	    (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
	    (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
	    (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
	    (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
	    (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))))))
;; Show quick tooltip
(use-package company-quickhelp
  :defines company-quickhelp-delay
  :bind (:map company-active-map
	      ("M-h" . company-quickhelp-manual-begin))
  :hook (global-company-mode . company-quickhelp-mode)
  :custom (company-quickhelp-delay 0.8))
;;; flycheck mode
(use-package flycheck
  :ensure t)

;;; yasnippet mode
(use-package yasnippet
  :ensure t)

;;; yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t)

;;; autoinsert
(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-minor-mode)
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))
(use-package autoinsert
  :init
  (setq auto-insert-query nil)

  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)

  :config
  (define-auto-insert "\\.c?$" ["default-c.c" autoinsert-yas-expand])
  (define-auto-insert "\\.h?$" ["default-h.h" autoinsert-yas-expand])
  (define-auto-insert "\\.html?$" "default-html.html"))

;;; abbrev
(use-package abbrev
  :ensure nil
  :delight
  :hook (text-mode . abbrev-mode)
  :custom (abbrev-file-name (concat mk-emacs-dir "abbrev_defs"))
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;;; flyspell
(use-package flyspell
  :ensure t
  :custom
  (flyspell-abbrev-p t)
  (flyspell-default-dictionary "en_US")
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil))

;;; flyspell-correct-helm
(use-package flyspell-correct-helm
  :ensure t
  :after (flyspell helm)
  :init (setq flyspell-correct-interface #'flyspell-correct-helm))

;;; langtool
(use-package langtool
  :ensure t
  :defer 2
  :delight
  :custom
  (langtool-default-language "en")
  (langtool-language-tool-jar "~/.local/apps/LanguageTool-4.5/languagetool-commandline.jar")
  (langtool-language-tool-server-jar "~/.local/apps/LanguageTool-4.5/languagetool-server.jar")
  (langtool-mother-tongue "ir"))

;;; lsp-mode
(use-package lsp-mode
  :ensure t
  :hook ((prog-mode) . lsp)
  :custom
  (lsp-prefer-flymake nil)
  (lsp-session-file (concat mk-backup-dir "lsp-session-v1"))
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
		    :major-modes '(python-mode)
		    :server-id 'pyls)))

(use-package lsp-ui
  :ensure t)

(use-package company-lsp
  :ensure t
  :diminish
  :after (company lsp-mode)
  :init
  (defvar company-lsp-enable-recompletion t)
  (defvar company-lsp-async t)
  :config
  (setq company-backends '(company-lsp company-yasnippet)))

(use-package company-c-headers
  :defer 7
  :hook ((c-mode c-common-mode c++-mode) . company-c-headers)
  :config
  (push 'company-c-headers company-backends))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

;;; packages.el ends here
