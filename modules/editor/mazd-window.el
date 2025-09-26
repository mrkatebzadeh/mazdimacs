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

(use-package shackle
  :ensure t
  :defer t
  :async (:priority low :packages(shackle))
  :config
  (setq shackle-rules
	'(("*xref*"                 :select t   :align right :size 0.4)
	  (" *undo-tree*"           :select t   :align right :size 0.3)
	  (magit-popup-mode         :select t   :align right :size 0.4)
	  (debugger-mode            :select t   :align below :size 0.4)
	  (magit-diff-mode          :select nil :align right :size 0.5)
	  (magit-log-select-mode    :select nil :align right :size 0.5)
	  ("*Ledger Report*"        :select t   :align below :size 0.5)
	  ("*org-roam*"             :select nil :align right :size 0.25)
	  (flycheck-error-list-mode :select nil :align below :size 0.25)
	  (vterm-mode               :select t   :align below :size 0.25)
	  (compilation-mode         :select nil :align below :size 0.25)
	  (comint-mode              :select nil :align below :size 0.25)
	  (messages-buffer-mode     :select t   :align below :size 0.25)
	  (inferior-emacs-lisp-mode :select t   :align below :size 0.25)
	  (ert-results-mode         :select t   :align below :size 0.5)
	  (calendar-mode            :select t   :align below :size 0.25)
	  (racer-help-mode          :select t   :align right :size 0.5)
	  (help-mode                :select t   :align right :size 0.5)
	  (helpful-mode             :select t   :align right :size 0.5)
	  ("CAPTURE-journal.org"    :select t   :align below :size 0.25)
	  (" *Embark Actions*"      :select nil :align below :size 0.5)
	  (" *Deletions*"           :select t   :align below :size 0.25)
	  (" *Marked Files*"        :select t   :align below :size 0.25)
	  ("*Org Select*"           :select t   :align below :size 0.33)
	  ("*Org Note*"             :select t   :align below :size 0.33)
	  ("*Org Links*"            :select t   :align below :size 0.2)
	  (" *Org todo*"            :select t   :align below :size 0.2)
	  ("*Man.*"                 :select t   :align below :size 0.5  :regexp t)
	  ("*Org Src.*"             :select t   :align right :size 0.5  :regexp t)
	  ("*Go-Translate*"         :select t   :align right :size 0.5)))
  )

(windmove-default-keybindings)
(winner-mode 1)


;;;###autoload
(defun mazd//switch-to-window-left ()
  "Switch to the window to the left and blink the buffer."
  (interactive)
  (evil-window-left 1)
  (mazd//blink-on-buffer-switch))

;;;###autoload
(defun mazd//switch-to-window-down ()
  "Switch to the window below and blink the buffer."
  (interactive)
  (evil-window-down 1)
  (mazd//blink-on-buffer-switch))

;;;###autoload
(defun mazd//switch-to-window-up ()
  "Switch to the window above and blink the buffer."
  (interactive)
  (evil-window-up 1)
  (mazd//blink-on-buffer-switch))

;;;###autoload
(defun mazd//switch-to-window-right ()
  "Switch to the window to the right and blink the buffer."
  (interactive)
  (evil-window-right 1)
  (mazd//blink-on-buffer-switch))

;;;###autoload
(defun mazd//vsplit-window ()
  "Vertically split window and blink the buffer."
  (interactive)
  (evil-window-vsplit)
  (mazd//blink-on-buffer-switch))

;;;###autoload
(defun mazd//split-window ()
  "Horizontally split window and blink the buffer."
  (interactive)
  (evil-window-split)
  (mazd//blink-on-buffer-switch))

;;;###autoload
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

;;;###autoload
(defun mazd//fullscreen ()
  "Set to fullscreen"
  (interactive)

  (set-frame-parameter nil 'fullscreen 'fullboth))


;;; bindings

(leader
  "wf" 'mazd//fullscreen
  "wv" 'mazd//vsplit-window
  "ws" 'mazd//split-window
  "wd" 'mazd//delete-window
  "wh" 'mazd//switch-to-window-left
  "wj" 'mazd//switch-to-window-down
  "wk" 'mazd//switch-to-window-up
  "wl" 'mazd//switch-to-window-right)

(leader
  "ti" 'imenu-list
  "td" 'darkroom-mode
  "tz" 'zoom-mode)

(provide 'mazd-window)
;;; mazd//window.el ends here
