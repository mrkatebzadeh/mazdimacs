;;; mazd//search.el --- Search -*- lexical-binding: t; -*-

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

(use-package engine-mode
  :defer t
  :config
  (defengine amazon
    "https://www.amazon.com/s?k=%s")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s")

  (defengine github
    "https://github.com/search?q=%s")

  (defengine google-images
    "https://www.google.com/search?tbm=isch&q=%s")

  (defengine google-maps
    "https://www.google.com/maps/search/%s")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s")

  (defengine youtube
    "https://www.youtube.com/results?search_query=%s")

  (defengine wikipedia
    "https://en.wikipedia.org/w/index.php?search=%s")

  (defengine google
    "https://www.google.com/search?q=%s")

  (defengine brave
    "https://search.brave.com/search?q=%s")

  )

(use-package google-translate
  :defer t
  :commands (google-translate-at-point)
  :custom (google-translate-default-target-language "fa"))

(leader
  "S" '(:ignore t :which-key "Search")
  "Sg" 'engine/search-google
  "Sb" 'engine/search-brave
  "St" 'google-translate-at-point
  "SA" 'engine/search-amazon
  "Si" 'engine/search-google-images
  "Sm" 'engine/search-google-maps
  "Ss" 'engine/search-stack-overflow
  "Sy" 'engine/search-youtube
  "Sw" 'engine/search-wikipedia
  "Sd" 'engine/search-duckduckgo
  "Sh" 'engine/search-github)

(provide 'mazd-search)
;;; mazd//search.el ends here
