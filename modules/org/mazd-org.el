;;; mazd-org.el<org> --- Org -*- lexical-binding: t; -*-

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

(use-package org
  :depends
  (calendar find-func format-spec org-macs org-compat org-faces org-entities
	    org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
	    org-capture)
  :ensure nil
  :defer t
  :mode ("\\.org$" . org-mode)
  :init
  (setq org-startup-with-inline-images t)
  :config
  (require 'org-id)
  (setq mazd//secret-dir (concat org-directory "/keys/")
	org-default-notes-file (concat org-directory "/agenda/notes.org"))

  (unless (file-exists-p org-directory)
    (make-directory org-directory))

  (mazd//setup-org-block-faces)
  )

(use-package evil-org
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme '(navigation insert textobjects)))))

(use-package org-bullets
  :disabled t
  :ensure t
  :defer t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "◉" "○" "✸" "✿")))

(use-package org-modern
  ;; :disabled t
  :ensure t
  :defer t
  :custom
  (org-modern-fold-stars
   '(
     ("⦿" . "⭘")
     (" ⬢" . " ⬡")
     ("  ✦" . "  ✧")
     ("    ➤" . "    ➢")
     ("     ☀" . "     ☼")))
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  )

(use-package org-contacts
  :ensure nil
  :defer t
  :after org
  :custom (org-contacts-files '((concat org-directory "/agenda/contacts.org"))))

(use-package org-faces
  :ensure nil
  :after org
  :defer t
  :custom
  (org-todo-keyword-faces
   '(("DONE" . (:foreground "cyan" :weight bold))
     ("SOMEDAY" . (:foreground "gray" :weight bold))
     ("TODO" . (:foreground "green" :weight bold))
     ("WAITING" . (:foreground "red" :weight bold)))))

(use-package pretty-symbols
  :ensure t
  :disabled t
  :config
  (setq pretty-symbol-categories '(relational logical lambda org-specific nil cpp general))

  (defun yant/str-to-glyph (str)
    "Transform string into glyph, displayed correctly."
    (let ((composition nil))
      (dolist (char (string-to-list str)
		    (nreverse (cdr composition)))
	(push char composition)
	(push '(Br . Bl) composition)
	)))

  (setq pretty-symbol-patterns
	(append pretty-symbol-patterns
		'((?▤ org-specific ":LOGBOOK:" (org-mode))
      		  (?⚙ org-specific ":PROPERTIES:" (org-mode))
      		  (?⏏ org-specific ":END:" (org-mode))
      		  (?★ org-specific "\\[#A\\]" (org-mode))
      		  ("" org-specific "\\[#B\\]" (org-mode))
      		  (?☕ org-specific "\\[#C\\]" (org-mode))
                  (?⁂ org-specific "\\(^\\*\\)[^*]" (org-mode) 1)
                  (?• org-specific "^\\(?:\\*\\{1\\}\\)\\(\\*\\)[^*]" (org-mode) 1)
                  (?⊢ org-specific "^\\(?:\\*\\{2\\}\\)\\(\\*\\)[^*]" (org-mode) 1)
                  (?⋮ org-specific "^\\(?:\\*\\{3\\}\\)\\(\\*\\)[^*]" (org-mode) 1)
                  (?⋱ org-specific "^\\(?:\\*\\{4,\\}\\)\\(\\*\\)[^*]" (org-mode) 1)
      		  ((yant/str-to-glyph "☐") org-specific "\\(?:^*+ +\\)\\(\\<TODO\\>\\)" (org-mode) 1)
      		  ((yant/str-to-glyph "☑") org-specific "\\(?:^*+ +\\)\\(\\<DONE\\>\\)" (org-mode) 1)
      		  ((yant/str-to-glyph "✘") org-specific "\\(?:^*+ +\\)\\(\\<FAILED\\>\\)" (org-mode) 1)
      		  ((yant/str-to-glyph "✘") org-specific "\\(?:^*+ +\\)\\(\\<CANCELLED\\>\\)" (org-mode) 1)
      		  ((yant/str-to-glyph "▶") org-specific "\\(?:^*+ +\\)\\(\\<NEXT\\>\\)" (org-mode) 1)
                  ((yant/str-to-glyph "☇") org-specific "\\(?:^*+ +\\)\\(\\<MERGED\\>\\)" (org-mode) 1)
      		  ((yant/str-to-glyph "⚑") org-specific "\\(?:^*+ +\\)\\(\\<WAITING\\>\\)" (org-mode) 1)
      		  ((yant/str-to-glyph "♲") org-specific "\\(?:^*+ +\\)\\(\\<HOLD\\>\\)" (org-mode) 1)
      		  ((yant/str-to-glyph "☠D") org-specific "\\<DEADLINE:" (org-mode))
      		  ((yant/str-to-glyph "◴S") org-specific "\\<SCHEDULED:" (org-mode))))))

(use-package org-modern-indent
  :disabled t
  :vc (:url "https://github.com/jdtsmith/org-modern-indent.git")
  :ensure nil
  :defer t
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(defun mazd//setup-org-block-faces ()
  "Set Org source block faces with dynamic colors based on the current background."
  (let* ((bg (mazd//get-bg-color))
         (block-bg (mazd//darken-color bg 20))
         (line-bg (mazd//lighten-color bg 20)))
    (custom-set-faces
     `(org-block ((t (:background ,block-bg :extend t :inherit fixed-pitch :margin 8))))
     `(org-block-begin-line ((t (:background ,line-bg :extend t :inherit fixed-pitch))))
     `(org-block-end-line ((t (:background ,line-bg :extend t :inherit fixed-pitch)))))))

(add-hook 'after-load-theme-hook #'mazd//setup-org-block-faces)

(defun mazd//org-export()
  "Load required packages for exporting org file"
  (interactive)
  (require 'ox-moderncv)
  (require 'ox-reveal))

(defun mazd//org-clicky()
  "Open link at point if there is one, otherwise insert newline."
  (interactive)
  (if (org-in-regexp org-link-any-re)
      (org-open-at-point)
    (org-return)))

(add-hook 'org-mode-hook
          (lambda ()
            (evil-define-key 'normal org-mode-map (kbd "RET") 'mazd//org-clicky)))
(advice-add 'org-open-at-point :before #'org-mark-ring-push)
(setq org-link-frame-setup '((file . find-file)))
(mazd//after org
  (define-key org-mode-map (kbd "C-c b") 'org-mark-ring-goto)
  (define-key org-mode-map (kbd "C-c f") 'org-mark-ring-push))


(defun mazd//eval-and-next-block ()
  "Evaluate the current Org mode source block and move to the next one."
  (interactive)
  (save-buffer)
  (org-babel-execute-src-block)
  (org-babel-next-src-block))

(defun mazd//org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (url (org-element-property :path link))
         (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

(defun mazd//org-link-open-eww ()
  "Open the Org mode link under the cursor directly in EWW."
  (interactive)
  (let* ((element (org-element-context))
         (type (org-element-property :type element))
         (path (org-element-property :path element))
         (link (concat type ":" path)))
    (if (and type path)
        (eww link)
      (message "No valid link found at point."))))

(defun insert-file-as-org-table (filename)
  "Insert a file into the current buffer at point, and convert it to an org table."
  (interactive (list (ido-read-file-name "csv file: ")))
  (let* ((start (point))
	 (end (+ start (nth 1 (insert-file-contents filename)))))
    (org-table-convert-region start end)))

(mazd//after ox-latex
  (add-to-list 'org-latex-classes
	       '("mazd-plain"
		 "\\documentclass[11pt]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage[english]{babel}
\\usepackage{newpxtext}
\\usepackage{microtype}
\\usepackage[margin=1in]{geometry}
\\usepackage{parskip}
\\usepackage{xcolor}
\\usepackage[
    colorlinks=true,
    linkcolor=blue,
    citecolor=darkblue,
    urlcolor=darkblue,
    pdfborder={0 0 0}
    ]{hyperref}
\\definecolor{darkblue}{rgb}{0.0, 0.0, 0.5}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}"))))
(setq org-latex-default-packages-alist nil)
(setq org-latex-packages-alist nil)
(setq org-latex-with-hyperref nil)


(leader
  "oe" 'mazd//org-export
  "oo" 'org-mode
  "oc" 'org-capture
  "ol" 'org-store-link
  )

(general-define-key
 :prefix "SPC k"
 :states '(normal visual motion)
 :keymaps 'org-mode-map
 "c" 'org-todo
 "s" 'org-schedule
 "n" 'mazd//eval-and-next-block
 "'" 'org-edit-special
 "d" 'org-deadline
 "D" 'org-insert-drawer
 "i" 'org-insert-structure-template
 "e" '(:ignore t :which-key "export")
 "ee" 'org-export-dispatch
 "f" 'org-set-effort
 "P" 'org-set-property
 ":" 'org-set-tags

 ;; "b" 'org-tree-to-indirect-buffer
 "A" 'org-archive-subtree
 "l" '(:ignore t :which-key "Link")
 "ly" 'mazd//org-link-copy
 "le" 'mazd//org-link-open-eww
 "lo" 'org-open-at-point
 "lb" 'org-mark-ring-goto
 "lf" 'org-mark-ring-push

 "T" 'org-show-todo-tree
 "." 'org-time-stamp
 "!" 'org-time-stamp-inactive

 ;; headings
 "h" '(:ignore t :which-key "Headings")
 "hi" 'org-insert-heading-after-current
 "hI" 'org-insert-heading
 "hs" 'org-insert-subheading

 ;; More cycling options (timestamps, headlines, items, properties)
 "L" 'org-shiftright
 "H" 'org-shiftleft
 "J" 'org-shiftdown
 "K" 'org-shiftup

 ;; Change between TODO sets
 "C-S-l" 'org-shiftcontrolright
 "C-S-h" 'org-shiftcontrolleft
 "C-S-j" 'org-shiftcontroldown
 "C-S-k" 'org-shiftcontrolup

 ;; Subtree editing
 "S" '(:ignore t :which-key "Subtree")
 "Sl" 'org-demote-subtree
 "Sh" 'org-promote-subtree
 "Sj" 'org-move-subtree-down
 "Sk" 'org-move-subtree-up

 ;; tables
 "T" '(:ignore t :which-key "Table")
 "Ta" 'org-table-align
 "Tb" 'org-table-blank-field
 "Tc" 'org-table-convert
 "Td" '(:ignore t :which-key "Delete")
 "Tdc" 'org-table-delete-column
 "Tdr" 'org-table-kill-row
 "Te" 'org-table-eval-formula
 "TE" 'org-table-export
 "Th" 'org-table-previous-field
 "TH" 'org-table-move-column-left
 "Ti" '(:ignore t :which-key "Insert")
 "Tic" 'org-table-insert-column
 "Tih" 'org-table-insert-hline
 "TiH" 'org-table-hline-and-move
 "Tir" 'org-table-insert-row
 "TI" 'org-table-import
 "Tj" 'org-table-next-row
 "TJ" 'org-table-move-row-down
 "TK" 'org-table-move-row-up
 "Tl" 'org-table-next-field
 "TL" 'org-table-move-column-right
 "Tn" 'org-table-create
 "TN" 'org-table-create-with-table.el
 "Tr" 'org-table-recalculate
 "Ts" 'org-table-sort-lines
 "Tt" '(:ignore t :which-key "Toggles")
 "Ttf" 'org-table-toggle-formula-debugger
 "Tto" 'org-table-toggle-coordinate-overlays
 "Tw" 'org-table-wrap-region

 "t" '(:ignore t :which-key "Toggles")
 "tc" 'org-toggle-checkbox
 "te" 'org-toggle-pretty-entities
 "ti" 'org-toggle-inline-images
 "tn" 'org-num-mode
 "tl" 'org-toggle-link-display
 "tt" 'org-show-todo-tree
 "tT" 'org-todo
 "tV" 'space-doc-mode
 "tx" 'org-latex-preview
 )
(provide 'mazd-org)
;;; mazd-org.el<org> ends here
