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

;;; Code:

(eval-when-compile
  (require 'mazd-vars)
  (require 'mazd-core))

;; evil
(use-package evil
  :ensure t
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
  (use-package evil-leader
    :ensure t
    :init (global-evil-leader-mode))
  (evil-mode 1)
  (use-package evil-tex
    :ensure t
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


;; which-key
(use-package which-key
  :ensure t
  :defer t
  :init
  (setq which-key-idle-delay 0.1)
  (which-key-mode))

(use-package multiple-cursors
  :ensure t
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

(use-package evil-mc
  :ensure t
  :defer t
  :commands (evil-mc-make-all-cursors
             evil-mc-undo-last-added-cursor
             evil-mc-undo-all-cursors
             evil-mc-pause-cursors
             evil-mc-resume-cursors
             evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-here
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-skip-and-goto-next-cursor
             evil-mc-skip-and-goto-prev-cursor
             evil-mc-skip-and-goto-next-match
             evil-mc-skip-and-goto-prev-match
             evil-mc-make-cursor-in-visual-selection-beg
             evil-mc-make-cursor-in-visual-selection-end)
  :init
  (el-patch-feature evil-mc)

  (el-patch-defvar evil-mc-cursors-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "m") #'evil-mc-make-all-cursors)
      (define-key map (kbd "u") #'evil-mc-undo-last-added-cursor)
      (define-key map (kbd "q") #'evil-mc-undo-all-cursors)
      (define-key map (kbd "s") #'evil-mc-pause-cursors)
      (define-key map (kbd "r") #'evil-mc-resume-cursors)
      (define-key map (kbd "f") #'evil-mc-make-and-goto-first-cursor)
      (define-key map (kbd "l") #'evil-mc-make-and-goto-last-cursor)
      (define-key map (kbd "h") #'evil-mc-make-cursor-here)
      (define-key map (kbd "j") #'evil-mc-make-cursor-move-next-line)
      (define-key map (kbd "k") #'evil-mc-make-cursor-move-prev-line)
      (define-key map (kbd "N") #'evil-mc-skip-and-goto-next-cursor)
      (define-key map (kbd "P") #'evil-mc-skip-and-goto-prev-cursor)
      (define-key map (kbd "n") #'evil-mc-skip-and-goto-next-match)
      (define-key map (kbd "p") #'evil-mc-skip-and-goto-prev-match)
      (define-key map (kbd "I") #'evil-mc-make-cursor-in-visual-selection-beg)
      (define-key map (kbd "A") #'evil-mc-make-cursor-in-visual-selection-end)
      map))

  (el-patch-defvar evil-mc-key-map
    (let ((map (make-sparse-keymap)))
      (evil-define-key* '(normal visual) map
        (kbd "gr") evil-mc-cursors-map
        (el-patch-remove
          (kbd "M-n") 'evil-mc-make-and-goto-next-cursor
          (kbd "M-p") 'evil-mc-make-and-goto-prev-cursor
          (kbd "C-n") 'evil-mc-make-and-goto-next-match
          (kbd "C-t") 'evil-mc-skip-and-goto-next-match
          (kbd "C-p") 'evil-mc-make-and-goto-prev-match))
      map))

  (global-set-key (kbd "C->") #'evil-mc-make-cursor-move-next-line)
  (global-set-key (kbd "C-<") #'evil-mc-make-cursor-move-prev-line)
  (define-key evil-normal-state-map (kbd "gr") evil-mc-cursors-map)

  :config
  (global-evil-mc-mode +1)

  (setq evil-mc-custom-known-commands
        '((evil-delete-backward-word-smart
           .
           ((:default . evil-mc-execute-default-call)))
          (end-of-visual-line-or-end
           .
           ((:default . evil-mc-execute-default-call)))
          (back-to-indentation-visual-or-beginning
           .
           ((:default . evil-mc-execute-default-call))))
        evil-mc-mode-line
        `(:eval
          (if (> (length evil-mc-cursor-list) 0)
              (evil-mc-active-mode-line (concat " " evil-mc-mode-line-prefix))
            "")))

  (el-patch-defun evil-mc-make-cursor-move-by-line (dir count)
    "Create COUNT cursors one for each line moving in the direction DIR.
DIR should be 1 or -1 and COUNT should be a positive integer or nil."
    (evil-force-normal-state)
    (setq count (max 0 (or count 1)))
    (dotimes (i count)
      (evil-mc-run-cursors-before)
      (evil-mc-make-cursor-at-pos (point))
      (let ((line-move-visual (el-patch-swap t nil)))
        (evil-line-move dir)))))


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
  "qq" 'mazd//kill-emacs
  "qQ" 'delete-frame)

(leader
  "te" 'global-evil-mc-mode)

(provide 'mazd-key)
;;; mazd//key.el ends here
