;;; theme
;; welcome screen
(use-package page-break-lines
  :ensure t)
(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-banner-logo-title "[-< True happiness can be found when two contrary powers cooperate together >-]")
  (setq dashboard-startup-banner "~/.emacs.d/logo.png")
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-items '((recents  . 7)
                          (projects . 5)
                          (agenda . 5)
                          (bookmarks . 10))))
;; theme
(use-package challenger-deep-theme
  :ensure t
  :init
  (load-theme 'challenger-deep t)
  :config)
;; all-the-icons
(use-package all-the-icons
  :ensure t
  :init
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))
;; spaceline
(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))
(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config
   (spaceline-all-the-icons-theme))
;; parentheses
;; highlight matches
(show-paren-mode 1)
;; auto-pair
(use-package autopair
  :ensure t
  :config
  (autopair-global-mode))
;; rainbow mode
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
;; transparency
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(85 . 50) '(100 . 100)))))
;; highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t
  :config
  (progn
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character)))
;;; Font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 95
                    :weight 'normal
                    :width 'normal)
;;; highlight current line
(global-hl-line-mode +1)
;; bars
(menu-bar-mode -1)
(toggle-scroll-bar -1) 
(tool-bar-mode -1) 
