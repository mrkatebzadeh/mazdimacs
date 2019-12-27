;;; mk-file.el --- File  -*- lexical-binding: t; -*-

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

(use-package projectile
  :defer t)

(use-package helm-projectile
  :defer t
  :commands (helm-projectile-switch-project
	     helm-projectile-find-file
	     helm-projectile-find-file-in-known-projects
	     helm-projectile-recentf
	     helm-projectile-ag)
  :config
  (helm-projectile-on))

(use-package recentf
  :defer t
  :init
  (add-hook 'find-file-hook (lambda () (unless recentf-mode
					 (recentf-mode)
					 (recentf-track-opened-file))))
  :custom
  (recentf-exclude (list "COMMIT_EDITMSG"
                         "~$"
                         "/scp:"
                         (expand-file-name mk-backup-dir)
                         (expand-file-name mk-local-dir)
                         (expand-file-name org-directory)
                         (expand-file-name (concat mk-emacs-dir "emms/"))
                         "/ssh:"
                         "/sudo:"
                         "/tmp/"))
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 15)
  (recentf-save-file (concat mk-backup-dir "recentf"))
  :config (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package docker-tramp
  :defer t)

(defun mk-kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
	  (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
	    (kill-buffer buffer)))
	(buffer-list)))

(use-package dired
  :ensure nil
  :config
  (evil-collection-init 'dired)
  (let ((args (list "-aBhl" "--group-directories-first")))
    (setq dired-listing-switches (string-join args " ")))
  (evil-define-key 'normal dired-mode-map (kbd "/") 'dired-narrow
    (kbd "P") 'peep-dired
    (kbd "t") 'dired-subtree-insert
    (kbd "T") 'dired-subtree-remove
    (kbd "q") 'mk-kill-dired-buffers)
  (require 'dired-rainbow)
  (evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down
    (kbd "C-<SPC>") 'peep-dired-scroll-page-up
    (kbd "<backspace>") 'peep-dired-scroll-page-up
    (kbd "j") 'peep-dired-next-file
    (kbd "k") 'peep-dired-prev-file)
  (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  :init
  (setq dired-auto-revert-buffer t
	dired-dwim-target t
	dired-hide-details-hide-symlink-targets nil
	;; Always copy/delete recursively
	dired-recursive-copies  'always
	dired-recursive-deletes 'top
	;; Where to store image caches
	image-dired-dir (concat mk-cache-dir "image-dired/")
	image-dired-db-file (concat image-dired-dir "db.el")
	image-dired-gallery-dir (concat image-dired-dir "gallery/")
	image-dired-temp-image-file (concat image-dired-dir "temp-image")
	image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")))

(use-package peep-dired
  :after dired
  :defer t
  :init
  (setq peep-dired-cleanup-on-disable t
	peep-dired-cleanup-eagerly t
	peep-dired-enable-on-directories t
	peep-dired-ignored-extensions '("mkv" "iso" "mp4")))

(use-package dired-narrow
  :after dired
  :defer t)

(use-package dired-subtree
  :after dired
  :defer t)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-rainbow
  :defer t
  :after dired
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html        "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml         "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document    "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown    "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database    "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media       "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image       "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log         "#c17d11" ("log"))
  (dired-rainbow-define shell       "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled    "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable  "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed  "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged    "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted   "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts       "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition   "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc          "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package treemacs
  :defer t)

(use-package treemacs-evil
  :after (treemacs evil)
  :defer t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :defer t)

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :defer t
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :defer t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :defer t)

;;; config
(with-eval-after-load 'projectile
  (setq projectile-globally-ignored-directories
        '(".bzr"
          ".ensime_cache"
          ".eunit"
          ".fslckout"
          ".git"
          ".hg"
          ".idea"
          ".stack-work"
          ".svn"
          ".tox"
          ".clangd"
          ".ccls-cache"
          "READONLY"
          "_FOSSIL_"
          "_darcs"
          "blaze-bin"
          "blaze-genfiles"
          "blaze-google3"
          "blaze-out"
          "blaze-testlogs"
          "node_modules"
          "third_party"
	  "backup"
          "vendor"))
  (setq projectile-completion-system 'helm
	projectile-enable-caching t
	projectile-switch-project-action 'helm-projectile-find-file)
  (projectile-global-mode))

(with-eval-after-load 'treemacs
  (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
	treemacs-deferred-git-apply-delay      0.5
	treemacs-display-in-side-window        t
	treemacs-eldoc-display                 t
	treemacs-file-event-delay              5000
	treemacs-file-follow-delay             0.2
	treemacs-follow-after-init             t
	treemacs-git-command-pipe              ""
	treemacs-goto-tag-strategy             'refetch-index
	treemacs-indentation                   2
	treemacs-indentation-string            " "
	treemacs-is-never-other-window         nil
	treemacs-max-git-entries               5000
	treemacs-missing-project-action        'ask
	treemacs-no-png-images                 nil
	treemacs-no-delete-other-windows       t
	treemacs-project-follow-cleanup        nil
	treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	treemacs-recenter-distance             0.1
	treemacs-recenter-after-file-follow    nil
	treemacs-recenter-after-tag-follow     nil
	treemacs-recenter-after-project-jump   'always
	treemacs-recenter-after-project-expand 'on-distance
	treemacs-show-cursor                   nil
	treemacs-show-hidden-files             t
	treemacs-silent-filewatch              nil
	treemacs-silent-refresh                nil
	treemacs-sorting                       'alphabetic-desc
	treemacs-space-between-root-nodes      t
	treemacs-tag-follow-cleanup            t
	treemacs-tag-follow-delay              1.5
	treemacs-width                         28)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
	       (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))


;;; bindings
(general-define-key
 :prefix "SPC"
 :states '(normal visual motion)
 :keymaps 'override
 "p" '(:ignore t :which-key "Projects"))

(general-define-key
 :prefix "SPC p"
 :states '(normal visual motion)
 :keymaps 'override
 "s" '(:ignore t :which-key "Search"))

(general-define-key
 :prefix "SPC p"
 :states '(normal visual motion)
 :keymaps 'override
 "a" 'helm-projectile-find-other-file
 "b" 'helm-projectile-switch-to-buffer
 "d" 'helm-projectile-find-dir
 "e" 'helm-projectile-recentf
 "f" 'helm-projectile-find-file
 "F" 'helm-projectile-find-file-in-known-projects
 "g" 'helm-projectile-find-file-dwim
 "i" 'projectile-invalidate-cache
 "p" 'helm-projectile-switch-project
 "r" 'helm-projectile-recentf
 "z" 'projectile-cache-current-file
 "sg" 'helm-projectile-grep
 "sa" 'helm-projectile-ack
 "ss" 'helm-projectile-ag)

(general-define-key
 :prefix "SPC a"
 :states '(normal visual motion)
 :keymaps 'override
 "d" 'dired)

(general-define-key
 :prefix "SPC t"
 :states '(normal visual motion)
 :keymaps 'override
 "f" 'treemacs)


(provide 'mk-file)
;;; mk-file.el ends here
