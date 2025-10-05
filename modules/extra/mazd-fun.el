;;; mazd//fun.el --- FUN  -*- lexical-binding: t; -*-

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

(use-package xkcd
  :ensure t
  :defer t
  :init
  (setq xkcd-cache-dir (concat mazd//local-dir "/xkcd/"))
  :config
  (setq xkcd-cache-latest (concat xkcd-cache-dir "latest"))
  (make-directory xkcd-cache-dir t))

(use-package zone-matrix
  :ensure (:host github :repo "ober/zone-matrix")
  :config
  (setq zone-programs [zone-matrix])
  (zone-when-idle 120)
  :custom-face
  (zmx-light-bar-body-face
   ((((class grayscale) (background dark))
     (:foreground "#8caaee"))
    (((class color) (min-colors 256))
     (:foreground "#8caaee"))
    (((class color) (min-colors 16))
     (:foreground "#8caaee"))
    (((class color) (min-colors 8))
     (:foreground "#8caaee"))
    (t (:inherit default :foreground "#8caaee"))))

  (zmx-light-bar-head-face-1
   ((((class grayscale) (background dark))
     (:weight bold :foreground "#81c8be"))
    (((class color) (min-colors 256))
     (:weight bold :foreground "#81c8be"))
    (((class color) (min-colors 16))
     (:weight bold :foreground "#81c8be"))
    (((class color) (min-colors 8))
     (:weight bold :foreground "#81c8be"))
    (t (:weight bold :inherit default :foreground "#81c8be"))))

  (zmx-light-bar-head-face-2
   ((((class grayscale) (background dark))
     (:weight bold :foreground "#babbf1"))
    (((class color) (min-colors 256))
     (:weight bold :foreground "#babbf1"))
    (((class color) (min-colors 16))
     (:weight bold :foreground "#babbf1"))
    (((class color) (min-colors 8))
     (:weight bold :foreground "#babbf1"))
    (t (:weight bold :inherit default :foreground "#babbf1"))))

  (zmx-light-bar-tail-face
   ((((class grayscale) (background dark))
     (:foreground "#f2cdcd" :weight light))
    (((class color) (min-colors 256))
     (:foreground "#f2cdcd" :weight light))
    (((class color) (min-colors 16))
     (:foreground "#f2cdcd" :weight light))
    (((class color) (min-colors 8))
     (:foreground "#f2cdcd" :weight light))
    (t (:inherit default :foreground "#f2cdcd" :weight light))))
  )

(leader
 "ax" 'xkcd)

(provide 'mazd-fun)
;;; mazd//fun.el ends here
