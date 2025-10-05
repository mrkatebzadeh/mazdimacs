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

(use-package general
  :ensure t
  :config
  (general-create-definer leader-map
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "A-SPC")
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

(eval-when-compile
  (setq evil-want-integration t
        evil-want-keybinding nil))

(use-package evil
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


  (add-hook 'LaTeX-mode-hook #'evil-tex-mode)
  (evil-set-initial-state 'TeX-error-overview-mode 'insert)
  ;; Redo
  (evil-set-undo-system 'undo-redo)
  (when (fboundp #'undo-tree-undo)
    (evil-set-undo-system 'undo-tree))


  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  )

;; evil-collection
(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init))

(use-package evil-goggles
  :ensure t
  :after (evil)
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
  (setq evil-goggles-duration 0.200)
  )

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

(use-package evil-quickscope
  :ensure t
  :after evil
  :config
  :hook ((prog-mode . turn-on-evil-quickscope-mode)
	 (LaTeX-mode . turn-on-evil-quickscope-mode)
	 (org-mode . turn-on-evil-quickscope-mode)))

(use-package evil-numbers
  :ensure t
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init
  (general-define-key
   :states 'motion
   "g+" 'evil-numbers/inc-at-pt
   "g-" 'evil-numbers/dec-at-pt))


(use-package kkp
  :ensure t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))

(use-package which-key
  :diminish t
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
  (setq mc/list-file (concat mazd//cache-dir "/.mc-lists.el"))
  (load mc/list-file t)

  (define-key mc/keymap (kbd "<return>") nil)
  (define-key mc/keymap (kbd "C-c <return>") 'multiple-cursors-mode))

(defmacro leader (&rest args)
  `(with-eval-after-load 'general
     (leader-map ,@args)))

(defvar local-leader-prefix "SPC k"
  "Default prefix key for local leader bindings.")

(defmacro local-leader (mode-map &rest args)
  "Define local leader keys for MODE-MAP with optional :prefix override."
  (let* ((prefix (if (plist-member args :prefix)
                     (plist-get args :prefix)
                   local-leader-prefix))
         ;; Remove :prefix and its value from args before passing to general
         (args (cl-loop for (k v) on args by #'cddr
                        unless (eq k :prefix) append (list k v))))
    `(with-eval-after-load 'general
       (general-define-key
        :prefix ,prefix
        :states '(normal visual motion)
        :keymaps ',mode-map
        ,@args))))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(elpaca-wait)

(leader
  "" '(nil :which-key "My lieutenant general prefix")
  "f" '(:ignore t :which-key "Files")
  "o" '(:ignore t :which-key "Org")
  "a" '(:ignore t :which-key "Applications")
  "g" '(:ignore t :which-key "Magit")
  "m" '(:ignore t :which-key "EMMS")
  "k" '(:ignore t :which-key "Local Bindings")
  "b" '(:ignore t :which-key "Buffers")
  "h" '(:ignore t :which-key "Help!")
  "v" '(:ignore t :which-key "Volume")
  "w" '(:ignore t :which-key "Windows")
  "t" '(:ignore t :which-key "Toggles")

  "x" 'execute-extended-command
  )

(provide 'mazd-key)
;;; mazd//key.el ends here
