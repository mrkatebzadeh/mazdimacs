;;; packages.el --- Docs -*- lexical-binding: t; -*-

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

;;; markdown-mode
(use-package markdown-mode
  :ensure t
  :delight "μ "
  :mode ("\\.markdown\\'" "\\.md\\'")
  :custom (markdown-command "/usr/bin/pandoc"))

;;; markdown-preview-mode
(use-package markdown-preview-mode
  :after markdown-mode
  :custom
  (markdown-preview-javascript
   (list (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/highlight.min.js")
         "<script>
            $(document).on('mdContentChange', function() {
              $('pre code').each(function(i, block)  {
                hljs.highlightBlock(block);
              });
            });
          </script>"))
  (markdown-preview-stylesheets
   (list (concat "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/"
                 "3.0.1/github-markdown.min.css")
         (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/styles/github.min.css")

         "<style>
            .markdown-body {
              box-sizing: border-box;
              min-width: 200px;
              max-width: 980px;
              margin: 0 auto;
              padding: 45px;
            }

            @media (max-width: 767px) { .markdown-body { padding: 15px; } }
          </style>")))

;;; nov
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :custom (nov-text-width 75))

;;; csv
(use-package csv-mode
  :ensure t)

;;; dockerfile-mode
(use-package dockerfile-mode
  :ensure t
  :delight "δ "
  :mode "Dockerfile\\'")

;;; elisp-mode
(use-package elisp-mode
  :ensure nil
  :delight "ξ ")

;;; eldoc
(use-package eldoc
  :ensure t
  :delight
  :hook (emacs-lisp-mode . eldoc-mode))

;;; rfc-mode
(use-package rfc-mode
  :ensure t
  :defer t)

;;; packages.el ends here
