;;; mazd-slide.el --- <TITLE> -*- lexical-binding: t; -*-

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

;;<CODE>

(use-package dslide
  :ensure t
  :vc (:url "https://github.com/positron-solutions/dslide")
  :defer t
  :commands (dslide-deck-start)
  :config
  (evil-define-key 'normal dslide-mode-map
    (kbd "<right>") 'dslide-deck-forward
    (kbd "<left>")  'dslide-deck-backward
    (kbd "<RET>")   'dslide-deck-present
    (kbd "<escape>") 'dslide-deck-stop)
  )

(use-package moc
  :ensure t
  :defer t
  :vc(:url "https://github.com/positron-solutions/moc.git"))

(use-package ox-reveal
  :defer t
  :ensure t
  :after org
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
	org-reveal-mathjax t)
  )

(use-package htmlize
  :ensure t
  :defer t)

(use-package gnuplot
  :ensure t
  :defer t)

(defun mazd//org-beamer-setup ()
  "Setup org-beamer"
  (unless (boundp 'org-export-latex-classes)
    (setq org-export-latex-classes nil))
  (add-to-list 'org-export-latex-classes
	       '("beamer"
		 "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"

		 ("\\section{%s}" . "\\section*{%s}")

		 ("\\begin{frame}[fragile]\\frametitle{%s}"
		  "\\end{frame}"
		  "\\begin{frame}[fragile]\\frametitle{%s}"
		  "\\end{frame}")))

  (add-to-list 'org-export-latex-classes

	       '("letter"
		 "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"

		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


  (setq org-latex-create-formula-image-program 'imagemagick)
  (setq org-latex-packages-alist
	'(("" "color" t)
          ("" "minted" t)
          ("" "parskip" t)
          ("" "tikz" t))))

(provide 'mazd-slide)
;;; mazd-slide.el ends here
