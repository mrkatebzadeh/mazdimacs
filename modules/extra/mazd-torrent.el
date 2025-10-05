;;; mazd//torrent.el --- Torrent  -*- lexical-binding: t; -*-

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

(use-package transmission
  :defer t)

(mazd//after transmission
  (defvar transmission-auto-refresh-all nil
    "Enable status auto refresh in all transmission buffers.")
  (setq transmission-refresh-modes '(transmission-mode
				     transmission-files-mode
				     transmission-info-mode
				     transmission-peers-mode)))
(leader
  "ab" 'transmission)

(mazd//after transmission
  (local-leader transmission-mode-map
		"" '(:ignore t :which-key "Transmission Mode")
		"a" 'transmission-add
		"d" 'transmission-set-download
		"e" 'transmission-peers
		"f" 'transmission-files
		"i" 'transmission-info
		"k" 'transmission-trackers-add
		"l" 'transmission-set-ratio
		"m" 'transmission-toggle-mark
		"r" 'transmission-remove
		"D" 'transmission-delete
		"s" 'transmission-toggle
		"t" 'transmission-invert-marks
		"u" 'transmission-set-upload
		"v" 'transmission-verify
		"q" 'transmission-quit
		"x" 'transmission-toggle
		"y" 'transmission-set-bandwidth-priority
		"U" 'transmission-unmark-all))


(provide 'mazd-torrent)
;;; mazd//torrent.el ends here
