;;; mazd//window.el --- Window  -*- lexical-binding: t; -*-

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

(use-package ace-window
  :ensure t
  :defer t
  :init
  (global-set-key [remap other-window] 'ace-window))

(use-package imenu
  :ensure nil
  :defer t)

(use-package imenu-list
  :ensure t
  :defer t
  :init
  (setq imenu-list-focus-after-activation t
	imenu-list-auto-resize t))

(use-package zoom
  :ensure t
  :defer t
  :init
					;  (zoom-mode t)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))))

(use-package darkroom
  :ensure t
  :defer t)

;;; config

(windmove-default-keybindings)
(winner-mode 1)


(defun mazd//switch-to-window-left ()
  "Switch to the window to the left and blink the buffer."
  (interactive)
  (evil-window-left 1)
  (mazd//blink-on-buffer-switch))

(defun mazd//switch-to-window-down ()
  "Switch to the window below and blink the buffer."
  (interactive)
  (evil-window-down 1)
  (mazd//blink-on-buffer-switch))

(defun mazd//switch-to-window-up ()
  "Switch to the window above and blink the buffer."
  (interactive)
  (evil-window-up 1)
  (mazd//blink-on-buffer-switch))

(defun mazd//switch-to-window-right ()
  "Switch to the window to the right and blink the buffer."
  (interactive)
  (evil-window-right 1)
  (mazd//blink-on-buffer-switch))

(defun mazd//vsplit-window ()
  "Vertically split window and blink the buffer."
  (interactive)
  (evil-window-vsplit)
  (mazd//blink-on-buffer-switch))

(defun mazd//split-window ()
  "Horizontally split window and blink the buffer."
  (interactive)
  (evil-window-split)
  (mazd//blink-on-buffer-switch))

(defun mazd//delete-window ()
  "Delete the current window and blink the buffer."
  (interactive)
  (evil-window-delete)
  (mazd//blink-on-buffer-switch))

(defun mazd//blink-buffer ()
  "Subtle blink effect for the entire buffer's background to indicate a switch."
  ;; (let* ((theme-color (if (eq (frame-parameter nil 'background-mode) 'dark)
  ;;                         "#45475a"
  ;;                       "#dce0e8"))
  ;;        (ov (make-overlay (point-min) (point-max))))
  ;;   (overlay-put ov 'face `(:background ,theme-color))
  ;;   (run-with-timer 0.15 nil #'delete-overlay ov))

  )

(defun mazd//blink-on-buffer-switch ()
  "Blink buffer when switching to a new one."
  (when (not (minibufferp))
    (mazd//blink-buffer)
    ))



;;; bindings

(leader
  "wv" 'mazd//vsplit-window
  "ws" 'mazd//split-window
  "wd" 'mazd//delete-window
  "wh" 'mazd//switch-to-window-left
  "wj" 'mazd//switch-to-window-down
  "wk" 'mazd//switch-to-window-up
  "wl" 'mazd//switch-to-window-right)

(leader
  "ti" 'imenu-list
  "tI" 'helm-imenu
  "td" 'darkroom-mode
  "tz" 'zoom-mode)

(provide 'mazd-window)
;;; mazd//window.el ends here
