;;; mazd-slack.el --- Slack -*- lexical-binding: t; -*-

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
					;
(use-package emacs-slack
  :defer t
  :commands (slack-register-team slack-start)
  :vc (:url "https://github.com/emacs-slack/emacs-slack")
  :ensure nil
  :bind (
         (:map slack-mode-map
               (("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-thread-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)
                ("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)))
         (:map slack-message-compose-buffer-mode-map
               (("C-c '" . slack-message-send-from-buffer)))
         )
  :config

  )

(use-package alert
  :ensure t
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))


;;;###autoload
(defun mazd//start-slack ()
  "Start Slack with default team"
  (interactive)
  (require 'emacs-slack)
  (slack-register-team
   :name (string-trim (mazd//read-encrypted-file "~/Dropbox/keys/slack/team.gpg"))
   :token (string-trim(mazd//read-encrypted-file "~/Dropbox/keys/slack/token.gpg"))
   :cookie (string-trim(mazd//read-encrypted-file "~/Dropbox/keys/slack/cookie.gpg"))
   :full-and-display-names t
   :default t
   :subscribed-channels nil
   )
  (slack-start)
  )

(local-leader slack-mode-map
	      "" '(:ignore t :which-key "Slack")
	      "K" 'slack-stop
	      "c" 'slack-select-rooms
	      "u" 'slack-select-unread-rooms
	      "U" 'slack-user-select
	      "s" 'slack-search-from-messages
	      "J" 'slack-jump-to-browser
	      "j" 'slack-jump-to-app
	      "e" 'slack-insert-emoji
	      "E" 'slack-message-edit
	      "r" 'slack-message-add-reaction
	      "t" 'slack-thread-show-or-create
	      "g" 'slack-message-redisplay
	      "G" 'slack-conversations-list-update-quick
	      "q" 'slack-quote-and-reply
	      "Q" 'slack-quote-and-reply-with-link
	      )

(leader
  "as" 'mazd//start-slack)


(provide 'mazd-slack)
;;; mazd-slack.el ends here
