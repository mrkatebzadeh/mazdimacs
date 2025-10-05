;;; mazd//email.el --- Email  -*- lexical-binding: t; -*-

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

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(use-package mu4e
  :async (:priority low
		    :packages (mu4e-core mu4e-headers mu4e-view mu4e-compose mu4e-alert mu4e-context))
  :ensure nil
  :defer t)

(use-package mu4e-conversation
  :ensure t
  :defer t
  :after mu4e)

(use-package mu4e-alert
  :ensure t
  :defer t
  :after mu4e)

(use-package consult-mu
  :ensure (:host github :repo "armindarvish/consult-mu")
  :after (consult mu4e)
  :custom
  (consult-mu-maxnum 200)
  (consult-mu-preview-key 'any)
  (consult-mu-mark-previewed-as-read nil)
  (consult-mu-mark-viewed-as-read t)
  (consult-mu-use-wide-reply t)
  (consult-mu-headers-template (lambda () (concat "%f" (number-to-string (floor (* (frame-width) 0.15))) "%s" (number-to-string (floor (* (frame-width) 0.5))) "%d13" "%g" "%x")))

  :config
  (setq consult-mu-saved-searches-dynamics '("#flag:unread"))
  (setq consult-mu-saved-searches-async '("#flag:unread"))
  (require 'consult-mu-embark)
  (require 'consult-mu-compose)
  (require 'consult-mu-compose-embark)
  (require 'consult-mu-contacts)
  (require 'consult-mu-contacts-embark)
  (setq consult-mu-compose-preview-key "M-o")
  (setq consult-mu-embark-attach-file-key "C-a")
  (setq consult-mu-contacts-ignore-list '("^.*no.*reply.*"))
  (setq consult-mu-contacts-ignore-case-fold-search t)
  (consult-mu-compose-embark-bind-attach-file-key)
  (setq consult-mu-compose-use-dired-attachment 'in-dired)
  )

(use-package notmuch
  :ensure t
  :defer t)

(use-package mu4e-column-faces
  :defer t
  :ensure t
  :config (mu4e-column-faces-mode))

(use-package org-mime
  :ensure t
  :defer t
  :after (org notmuch)
  :config (setq org-mime-library 'mml))

;;;###autoload
(defun mazd//mu4e()
  (interactive)
  (require 'mu4e)
  (mu4e-column-faces-mode)
  (mu4e))

(mazd//after mu4e
  (plist-put (cdr (assq 'refile   mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'trash    mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'untrash  mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'unread   mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'delete   mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'flag     mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'unflag   mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'move     mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'tag      mu4e-marks)) :char "×")

  (defface mu4e-headers-svg-tag-default-face
    `((t :foreground "#434555"
	 :background "#ff3344"
	 :box (:line-width 1 :color "#445566" :style nil)
	 :family ,(face-attribute 'default :family)
	 :weight ,(face-attribute 'default :weight)
	 :height ,(if (display-graphic-p)
                      (- (face-attribute 'default :height) 20)
                    1)))
    "SVG tag default face"
    :group 'svg-tag)
  ;; Folding mode for mu4e
  (use-package mu4e-thread-folding
    :ensure nil)

  (require 'svg-tag-mode)
  (require 'mu4e-thread-folding)
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
				 :shortname ""
				 :function (lambda (msg) "  "))))
  (setq mu4e-headers-fields '((:empty         .    2)
                              (:human-date    .   12)
                              (:flags         .    6)
                              (:mailing-list  .   10)
                              (:from          .   22)
                              (:subject       .   nil)))
  (setq mu4e-maildir "~/.local/share/maildir"
	mu4e-get-mail-command (concat (executable-find "mbsync" ) " -a --verbose")
	mu4e-update-interval 300
	mu4e-view-show-images t
	mu4e-view-show-addresses t
	mu4e-enable-notifications nil
	mu4e-enable-mode-line t
	mu4e-headers-skip-duplicates t
	mu4e-change-filenames-when-moving t
	mu4e-view-image-max-width 800
	mu4e-completing-read-function 'completing-read
	mu4e-use-fancy-chars 't
	mu4e-view-show-images 't
	message-kill-buffer-on-exit 't
	mu4e-maildir-shortcuts
	'(("/gmail/INBOX" . ?g)
	  ("/uoe/INBOX" . ?s)))
  (let ((dir "~/Downloads"))
    (when (file-directory-p dir)
      (setq mu4e-attachment-dir dir)))
  (add-hook 'mu4e-compose-mode-hook
	    (lambda () (use-hard-newlines t 'guess)))
  (setq mu4e-view-actions
	'(("capture message" . mu4e-action-capture-message)
          ("view in browser" . mu4e-action-view-in-browser)
          ("show this thread" . mu4e-action-show-thread)
	  ("View as pdf" . mu4e-action-view-as-pdf)))
  ;; default send
  (setq mu4e-sent-folder "/gmail/[Gmail]/Sent Mail"
	mu4e-drafts-folder "/gmail/[Gmail]/Drafts"
	mu4e-trash-folder "/gmail/[Gmail]/Trash"
	user-mail-address "mr.katebzadeh@gmail.com"
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587)
  ;; list of accounts
  (defvar mu4e-account-list
    '(("gmail"
       (mu4e-sent-folder "/gmail/[Gmail]/Sent Mail")
       (mu4e-drafts-folder "/gmail/[Gmail]/Drafts")
       (mu4e-trash-folder "/gmail/[Gmail]/Trash")
       (user-mail-address "mr.katebzadeh@gmail.com")
       (user-full-name "M.R. Siavash Katebzadeh")
       )
      ("uoe"
       (mu4e-sent-folder "/uoe/Sent")
       (mu4e-drafts-folder "/uoe/Drafts")
       (mu4e-trash-folder "/uoe/Trash")
       (user-mail-address "m.r.katebzadeh@ed.ac.uk")
       (user-full-name "M.R. Siavash Katebzadeh")
       )))
  ;; set account
  (defun mu4e-set-account ()
    (let* ((account
	    (if mu4e-compose-parent-message
		(let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
		  (string-match "/\\(.*?\\)/" maildir)
		  (match-string 1 maildir))
	      (completing-read (format "Compose with account: (%s) "
				       (mapconcat #'(lambda (var) (car var))
						  mu4e-account-list "/"))
			       (mapcar #'(lambda (var) (car var)) mu4e-account-list)
			       nil t nil nil (caar mu4e-account-list))))
	   (account-vars (cdr (assoc account mu4e-account-list))))
      (if account-vars
	  (mapc #'(lambda (var)
		    (set (car var) (cadr var)))
		account-vars)
	(error "No email account found"))))
  (add-hook 'mu4e-compose-pre-hook 'mu4e-set-account)
  ;; msmtp
  (setq message-send-mail-function 'message-send-mail-with-sendmail
	sendmail-program (executable-find "msmtp")
	user-full-name "M.R. Siavash Katebzadeh")
  (defun choose-msmtp-account ()
    (if (message-mail-p)
	(save-excursion
	  (let*
	      ((from (save-restriction
		       (message-narrow-to-headers)
		       (message-fetch-field "from")))
	       (account
		(cond
		 ((string-match "mr.katebzadeh@gmail.com" from) "gmail")
		 ((string-match "mrkatebzadeh.com" from) "gmail")
		 ((string-match "m.r.katebzadeh@ed.ac.uk" from) "uoe")
		 ((string-match "kmohamma@ed.ac.uk" from) "staff")
		 ((string-match "m.r.katebzadeh@ed.ac.uk" from) "uoe"))))
	    (setq message-sendmail-extra-arguments (list '"-a" account))))))
  (setq message-sendmail-envelope-from 'header)
  (add-hook 'message-send-mail-hook 'choose-msmtp-account)

;;;###autoload
  (defun mu4e-choose-signature ()
    "Insert one of a number of sigs"
    (interactive)
    (let ((message-signature
	   (mu4e-read-option "Signature:"
			     '(("formal" .
				(concat
				 "Best regards,\n"
				 "M.R. Siavash Katebzadeh\n"
				 ))
			       ("informal" .
				"Best,\nSiavash\n")))))
      (message-insert-signature)))
  (add-hook 'mu4e-compose-mode-hook
	    (lambda () (local-set-key (kbd "C-c C-w") #'mu4e-choose-signature)))
  (require 'mu4e-org)
  (setq org-mu4e-link-query-in-headers-mode nil))

(mazd//after mu4e-alert
  (mu4e-alert-set-default-style 'notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))

;;;###autoload
(defun mazd//notmuch-show-expand-only-unread-h ()
  (interactive)
  (let ((unread nil)
        (open (notmuch-show-get-message-ids-for-open-messages)))
    (notmuch-show-mapc (lambda ()
                         (when (member "unread" (notmuch-show-get-tags))
                           (setq unread t))))
    (when unread
      (let ((notmuch-show-hook (remove 'mazd//notmuch-show-expand-only-unread-h notmuch-show-hook)))
        (notmuch-show-filter-thread "tag:unread")))))

(defun mazd//notmuch-dont-confirm-on-kill-process-a (orig-fn &rest args)
  "Don't prompt for confirmation when killing notmuch sentinel."
  (let (confirm-kill-processes)
    (apply orig-fn args)))

(mazd//after notmuch
  (setq notmuch-fcc-dirs nil
	notmuch-show-logo nil
	notmuch-message-headers-visible nil
	message-kill-buffer-on-exit t
	message-send-mail-function 'message-send-mail-with-sendmail
	notmuch-search-oldest-first nil
	send-mail-function 'sendmail-send-it
	sendmail-program "/usr/local/bin/msmtp"
	notmuch-search-result-format
	'(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-30s ")
          ("subject" . "%-72s ")
          ("tags" . "(%s)"))
	notmuch-tag-formats
	'(("unread" (propertize tag 'face 'notmuch-tag-unread)))
	notmuch-hello-sections
	'(notmuch-hello-insert-saved-searches
          notmuch-hello-insert-alltags)
	notmuch-saved-searches
	'((:name "inbox"   :query "tag:inbox not tag:trash" :key "i")
          (:name "flagged" :query "tag:flagged"             :key "f")
          (:name "sent"    :query "tag:sent"                :key "s")
          (:name "drafts"  :query "tag:draft"               :key "d"))
	notmuch-archive-tags '("-inbox" "-unread"))
  (add-hook 'notmuch-show-hook #'mazd//notmuch-show-expand-only-unread-h)

  (add-hook 'doom-real-buffer-functions #'notmuch-interesting-buffer)

  (advice-add #'notmuch-start-notmuch-sentinel :around #'mazd//notmuch-dont-confirm-on-kill-process-a)
  )

(mazd//after  mu4e
  (local-leader mu4e-compose-mode-map
		"k" 'message-kill-buffer
		"w" 'message-send
		"s" 'mu4e-choose-signature
		"c" 'consult-mu-contacts
		"a" 'consult-mu-compose-attach
		"d" 'consult-mu-compose-detach
		)
  (evil-collection-init 'mu4e)

  (evil-define-key 'normal mu4e-headers-mode-map (kbd "/") 'consult-mu-async)
  (evil-define-key 'normal mu4e-headers-mode-map (kbd "C") 'consult-mu)
  )
(leader
 "am" 'mazd//mu4e
 "an" 'notmuch)

(provide 'mazd-email)
;;; mazd//email.el ends here
