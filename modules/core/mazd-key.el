;;; mazd//key.el --- Key -*- lexical-binding: t; -*-

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

(use-package evil
  :defer t
  :ensure t
  :hook(text-mode . evil-mode)
  :init
  (setq evil-search-module 'evil-search
	evil-ex-complete-emacs-commands nil
	evil-vsplit-window-right t
	evil-split-window-below t
	evil-shift-round nil
	evil-want-C-u-scroll t
	evil-default-cursor t
	evil-want-integration nil
	evil-want-keybinding nil)
  ;; This has to be before we invoke evil-mode due to:
  ;; https://github.com/cofi/evil-leader/issues/10
  ;; (use-package evil-leader
  ;; :ensure t
  ;; :init (global-evil-leader-mode))
  (evil-mode 1)
(use-package evil-tex
  :ensure t
  :defer t
  :after(evil latex-mode)
  :commands (evil-tex-mode)
  :config
  (defun mazd//evil-tex-toggle-math ()
    "Toggle surrounding math between \\(foo\\) and \\=\\[foo\\]."
    (interactive)
    (let* ((outer (evil-tex-a-math)) (inner (evil-tex-inner-math))
           (left-over (make-overlay (car outer) (car inner)))
           (right-over (make-overlay (cadr inner) (cadr outer))))
      (save-excursion
        (goto-char (overlay-start left-over))
        (cond
         ((looking-at (regexp-quote "\\("))
          (evil-tex--overlay-replace left-over  "\\[")
          (evil-tex--overlay-replace right-over "\\]" )
          (goto-char (overlay-end right-over))
          (when (looking-at (rx punct))
            (let ((match (match-string 0)))
              (delete-char 1)
              (goto-char (overlay-start right-over))
              (insert match))))
         ((looking-at (regexp-quote "\\["))
          (evil-tex--overlay-replace left-over  "\\(")
          (evil-tex--overlay-replace right-over "\\)" )
          (goto-char (overlay-start right-over))
          (when (looking-back (rx punct) 1)
            (let ((match (match-string 0)))
              (delete-char -1)
              (goto-char (overlay-end right-over))
              (insert match))))))
      (delete-overlay left-over) (delete-overlay right-over)))

  (define-key evil-tex-toggle-map (kbd "m") #'mazd//evil-tex-toggle-math))

(add-hook 'LaTeX-mode-hook #'evil-tex-mode)
(evil-set-initial-state 'TeX-error-overview-mode 'insert)

)

;; evil-collection
(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init))

;; Display visual hint on evil edit operations
(use-package evil-goggles
  :ensure t
  :defer
  :after(evil)
  :hook(text-mode . evil-goggles-mode)
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

;; general
(use-package general
  :ensure t
  :config
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (general-override-mode)
  (general-auto-unbind-keys)
  )

(use-package kkp
  :ensure t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))

(use-package which-key
  :ensure t
  :defer t
  :init
  (setq which-key-idle-delay 0.1)
  (which-key-mode))

(use-package multiple-cursors
  :ensure t
  :defer t
  :commands (mc/mark-lines
             mc/mark-next-lines
             mc/mark-previous-lines
             mc/unmark-next-like-this
             mc/unmark-previous-like-this
             mc/skip-to-previous-like-this
             mc/mark-all-like-this
             mc/mark-all-words-like-this
             mc/mark-all-symbols-like-this
             mc/mark-all-in-region
             mc/mark-all-in-region-regexp
             mc/mark-more-like-this-extended
             mc/mmlte--up
             mc/mmlte--down
             mc/mmlte--left
             mc/mmlte--right
             mc/mark-all-like-this-dwim
             mc/mark-all-dwim
             mc/mark-all-like-this-in-defun
             mc/mark-all-words-like-this-in-defun
             mc/mark-all-symbols-like-this-in-defun
             mc/add-cursor-on-click
             mc/mark-sgml-tag-pair
             mc/mark-pop
             set-rectangular-region-anchor
             rrm/switch-to-multiple-cursors
             mc/insert-numbers
             mc/reverse-regions
             mc/sort-regions
             hum/keyboard-quit
             mc-hide-unmatched-lines-mode)
  :config
  (setq mc/list-file (locate-user-emacs-file ".cache/.mc-lists.el"))

  ;; This is required to load the save file, due to a poor design
  ;; decision in multiple-cursors.el
  (load mc/list-file t)

  (define-key mc/keymap (kbd "<return>") nil)
  (define-key mc/keymap (kbd "C-c <return>") 'multiple-cursors-mode))

(general-create-definer leader
  :states '(normal visual emacs)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "A-SPC")

;; Redo
(evil-set-undo-system 'undo-redo)
;; Esc
;;;(global-set-key [escape] 'keyboard-quit)
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun mazd//kill-emacs ()
  "Check for unsaved file buffers before exiting Emacs."
  (interactive)
  (let ((unsaved-buffers
         (cl-loop for buf in (buffer-list)
                  for filename = (buffer-file-name buf)
                  when (and filename (buffer-modified-p buf))
                  collect buf)))
    (if unsaved-buffers
        (if (y-or-n-p (format "There are %d unsaved file buffers. Save them before exiting? " (length unsaved-buffers)))
            (progn
              (save-some-buffers t)  ;; Save all unsaved buffers
              (kill-emacs))          ;; Then exit Emacs
          (message "Exit aborted."))
      (kill-emacs))))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; leader file
(leader
  "" '(nil :which-key "My lieutenant general prefix")
  "f" '(:ignore t :which-key "Files")
  "c" '(:ignore t :which-key "Config Files")
  "o" '(:ignore t :which-key "Org")
  "a" '(:ignore t :which-key "Applications")
  "g" '(:ignore t :which-key "Magit")
  "m" '(:ignore t :which-key "EMMS")
  "l" '(:ignore t :which-key "Local Bindings")
  "b" '(:ignore t :which-key "Buffers")
  "h" '(:ignore t :which-key "Help!")
  "v" '(:ignore t :which-key "Volume")
  "w" '(:ignore t :which-key "Windows")
  "q" '(:ignore t :which-key "Quit")
  "t" '(:ignore t :which-key "Toggles")

  "x" 'execute-extended-command
  )

;; Exit/restart/reboot/shutdown
(leader
  "qQ" 'mazd//kill-emacs
  "qq" 'delete-frame)

(provide 'mazd-key)
;;; mazd//key.el ends here
