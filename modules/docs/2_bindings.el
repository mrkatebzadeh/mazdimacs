;;; bindings.el --- Docs -*- lexical-binding: t; -*-

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
(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'markdown-mode-map
 ;; Movement
 "{"   'markdown-backward-paragraph
 "}"   'markdown-forward-paragraph
 ;; Completion, and Cycling
 "]"   'markdown-complete
 ;; Indentation
 ">"   'markdown-indent-region
 "<"   'markdown-exdent-region
 ;; Buffer-wide commands
 "c]"  'markdown-complete-buffer
 "cc"  'markdown-check-refs
 "ce"  'markdown-export
 "cm"  'markdown-other-window
 "cn"  'markdown-cleanup-list-numbers
 "co"  'markdown-open
 "cp"  'markdown-preview
 "cv"  'markdown-export-and-preview
 "cw"  'markdown-kill-ring-save
 ;; headings
 "hi"  'markdown-insert-header-dwim
 "hI"  'markdown-insert-header-setext-dwim
 "h1"  'markdown-insert-header-atx-1
 "h2"  'markdown-insert-header-atx-2
 "h3"  'markdown-insert-header-atx-3
 "h4"  'markdown-insert-header-atx-4
 "h5"  'markdown-insert-header-atx-5
 "h6"  'markdown-insert-header-atx-6
 "h!"  'markdown-insert-header-setext-1
 "h@"  'markdown-insert-header-setext-2
 ;; Insertion of common elements
 "-"   'markdown-insert-hr
 "if"  'markdown-insert-footnote
 "ii"  'markdown-insert-image
 "ik"  'spacemacs/insert-keybinding-markdown
 "iI"  'markdown-insert-reference-image
 "il"  'markdown-insert-link
 "iL"  'markdown-insert-reference-link-dwim
 "iw"  'markdown-insert-wiki-link
 "iu"  'markdown-insert-uri
 ;; Element removal
 "k"   'markdown-kill-thing-at-point
 ;; List editing
 "li"  'markdown-insert-list-item
 ;; region manipulation
 "xb"  'markdown-insert-bold
 "xi"  'markdown-insert-italic
 "xc"  'markdown-insert-code
 "xC"  'markdown-insert-gfm-code-block
 "xq"  'markdown-insert-blockquote
 "xQ"  'markdown-blockquote-region
 "xp"  'markdown-insert-pre
 "xP"  'markdown-pre-region
 ;; Following and Jumping
 "N"   'markdown-next-link
 "f"   'markdown-follow-thing-at-point
 "P"   'markdown-previous-link
 "<RET>" 'markdown-jump)

;;; rfc-mode
(general-define-key
 :prefix "SPC a"
 :states '(normal visual motion)
 :keymaps 'override
 "R" 'rfc-mode-init)

(general-define-key
 :prefix "SPC l"
 :states '(normal visual motion)
 :keymaps 'rfc-mode-map
 "g" 'rfc-mode-browse
 "l" 'rfc-mode-read
 "u" 'rfc-update
 "q" 'rfc-mode-quit
 "n" 'rfc-mode-forward-page
 "p" 'rfc-mode-backward-page)

;;; bindings.el ends here
