;;; mazd-splash.el --- <TITLE> -*- lexical-binding: t; -*-

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
(require 'dash)

(defvar mazd//max-rows 300
  "Maximum number of rows a window can have")

(defvar mazd//max-columns 500
  "Maximum number of columns a window can have")


(defvar mazd//banner
  "███╗   ███╗ █████╗ ███████╗██████╗ ██╗███╗   ███╗ █████╗  ██████╗███████╗
████╗ ████║██╔══██╗╚══███╔╝██╔══██╗██║████╗ ████║██╔══██╗██╔════╝██╔════╝
██╔████╔██║███████║  ███╔╝ ██║  ██║██║██╔████╔██║███████║██║     ███████╗
██║╚██╔╝██║██╔══██║ ███╔╝  ██║  ██║██║██║╚██╔╝██║██╔══██║██║     ╚════██║
██║ ╚═╝ ██║██║  ██║███████╗██████╔╝██║██║ ╚═╝ ██║██║  ██║╚██████╗███████║
╚═╝     ╚═╝╚═╝  ╚═╝╚══════╝╚═════╝ ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
-< True happiness is found when two opposed powers cooperate together! >-

"
  "Banner to display on startup.")

(defvar mazd//box-dimensions nil
  "Variable used to store dimensions (rows columns) of banner text.")

;; calculate width of input text
(defun mazd//banner-box-dimenions ()
  "Returns list (row col) giving dimensions of bounding box of
  mazd//banner."
  (let* ((strings (split-string mazd//banner "\n"))
	 (string-lengths (-map 'length strings))
	 (ncol (apply 'max string-lengths))
	 (nrow (length strings)))
    (setq mazd//box-dimensions (list nrow ncol))))

(defvar mazd//insertion-point nil
  "Variable used to store insertion point of upper left point of banner.")

(defun mazd//set-local-vars ()
  "Internal function used to set all the local variables for the mode."
  (display-line-numbers-mode 0)
  (if truncate-lines
      (toggle-truncate-lines 1))
  (visual-line-mode -1)
  (setq-local auto-hscroll-mode nil)
  (setq-local hscroll-margin 0)
  (setq left-fringe-width 0)
  (setq right-fringe-width 0)
  (set-display-table-slot standard-display-table 'truncation 32)
  (set-window-buffer (selected-window) (get-buffer "*splash*"))
  (setq cursor-type nil)
  (face-remap-add-relative 'region '(:inherit default))
  (if (fboundp 'evil-mode)
      (setq-local evil-normal-state-cursor nil)
    (setq-local evil-emacs-state-cursor nil)
    (setq-local cursor-type nil)))

(define-derived-mode mazd//mode
  fundamental-mode "MAZD"
  "Major mode for showing custom splash screen."
  ;; bit of setup to make display nice
  (mazd//set-local-vars))

;; make buffer of size equal to largest monitor store center of text coordinates
(defun mazd//make-splash-buffer ()
  "Creates buffer of dimension `mazd//max-columns' and `mazd//max-rows' and places the
banner at the center. Also checks to see if buffer named *splash* already exists and if so overwrites it"
  (unless mazd//box-dimensions
    (mazd//banner-box-dimenions))
  (let* ((splash-buffer (get-buffer-create "*splash*"))
	 (height (/ mazd//max-rows 2))
	 (width (/ mazd//max-columns 2))
	 (box-top (/ (car mazd//box-dimensions) 2))
	 (box-left (/ (nth 1 mazd//box-dimensions) 2))
	 (padding-top (- height box-top))
	 (padding-left (- width box-left))
	 (top-pad-string (concat (make-string mazd//max-columns ?\s) "\n")))
    (switch-to-buffer splash-buffer)
    (read-only-mode -1)
    (mazd//set-local-vars)
    (if (string= major-mode "mazd//mode")
	(erase-buffer)
      nil)
    (dotimes (_ padding-top) (insert top-pad-string))
    (let ((tmp-point (point))
	  (indent-tabs-mode nil))
      (insert mazd//banner)
      (mark-paragraph)
      (indent-rigidly (point) (mark) padding-left)
      (goto-char (point))
      (re-search-forward "[^\s\n]") ; FIXME
      (backward-char)
      (setq mazd//insertion-point (point))
      (deactivate-mark)
      (read-only-mode 1)
      (mazd//mode)
      (get-buffer "*splash*"))))


(defun mazd//set-window-start (window)
  "Set window start to center banner in `window'."
  ;; look at set-window-start function
  (let* ((height (window-body-height nil))
	 (width (window-total-width nil))
	 (box-top (/ (nth 0 mazd//box-dimensions) 2))
	 (box-left (/ (nth 1 mazd//box-dimensions) 2))
	 (calling-window (selected-window)))
    (select-window window)
    (mazd//set-local-vars)
    ;; now acctually set window start
    (goto-char mazd//insertion-point)
    (set-window-start (selected-window) (point) nil)
    (scroll-left (- (current-column) (window-hscroll)))
    (scroll-down (+ 1 (- (/ height 2) box-top)))
    (scroll-right (- (/ width 2) box-left))
    (select-window calling-window)))


(defun mazd//redraw ()
  (interactive)
  "Fix up buffer and recenter."
  (mazd//make-splash-buffer)
  (mazd//set-window-start (selected-window)))


(defun mazd//recenter ()
  (interactive)
  "Fix up buffer and recenter."
  ;; TODO: if buffer is mazd
  (if (string= major-mode "mazd//mode")
      (mazd//set-window-start (selected-window))))

(defun mazd//initial-buffer ()
  "Function designed to be called by initial buffer."
  (mazd//redraw)
  (get-buffer "*splash*"))

(defun mazd//window-size-change-function (arg)
  "Funtion to run on window size change."
  (when (get-buffer "*splash*")
    ;; (if (string= major-mode "mazd//mode")
    ;; 	(mazd//set-window-start (selected-window)))
    (let ((w-to-update (get-buffer-window-list "*splash*" nil (selected-frame))))
      (-map 'mazd//set-window-start w-to-update))
    ))

(add-hook 'window-size-change-functions 'mazd//window-size-change-function)
(setq initial-buffer-choice 'mazd//initial-buffer)

(provide 'mazd-splash)
;;; mazd-splash.el ends here
