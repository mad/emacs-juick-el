;;; juick.el --- improvement reading juick@juick.com

;; Copyright (C) 2009  mad

;; Author: mad <owner.mad.epa@gmail.com>
;; Keywords: juick

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

;; Markup message recivied from juick@juick.com and some usefull keybindings.

;;; Installing:

;; 1. Put juick.el to you load-path
;; 2. put this to your init file:

;; (require 'juick)

;; and any useful settings

;; (setq juick-tag-subscribed '("linux" "juick" "jabber" "emacs" "vim"))
;; (setq juick-auto-subscribe-list '("linux" "emacs" "vim" "juick" "ugnich"))
;; (juick-auto-update t)

;;; Default bind:

;; u - unsubscribe message/user
;; s - subscribe message/user
;; d - delete message
;; b - bookmark message/user
;; p - make private message with user
;; C-cjb - `juick-bookmark-list'
;; TAB - `juick-next-button'

;;; Code:

(require 'button)

;; XXX: if jabber load through `jabber-autloads'
(require 'jabber-chatbuffer)
(require 'jabber-ibb)
(require 'jabber-geoloc)
(require 'jabber-tune)

(require 'google-maps)

(defgroup juick-faces nil "Faces for displaying Juick msg"
  :group 'juick)

(defface juick-id-face
  '((t (:weight bold)))
  "face for displaying id"
  :group 'juick-faces)

(defface juick-user-name-face
  '((t (:foreground "blue" :weight bold :slant normal)))
  "face for displaying user name"
  :group 'juick-faces)

(defface juick-tag-face
  '((t (:foreground "red4" :slant italic)))
  "face for displaying tags"
  :group 'juick-faces)

(defface juick-bold-face
  '((t (:weight bold :slant normal)))
  "face for displaying bold text"
  :group 'juick-faces)

(defface juick-italic-face
  '((t (:slant italic)))
  "face for displaying italic text"
  :group 'juick-faces)

(defface juick-underline-face
  '((t (:underline t :slant normal)))
  "face for displaying underline text"
  :group 'juick-faces)

(defvar juick-reply-id-add-plus nil
  "Set to t then id inserted with + (e.g NNNN+).

Useful for people more reading instead writing")

(defvar juick-overlays nil)

(defvar juick-bot-jid "juick@juick.com")

(defvar juick-image-buffer "*juick-avatar-dir*")

(defvar juick-point-last-message nil)

(defvar juick-avatar-internal-stack nil
  "Internal var")

(defvar juick-avatar-update-day 4
  "Update (download) avatar after `juick-avatar-update-day'")

(defvar juick-icon-mode t
  "This mode display avatar in buffer chat")

(defvar juick-icon-hight nil
  "If t then show 96x96 avatars")

(defvar juick-tag-subscribed '()
  "List subscribed tags")

(defvar juick-auto-subscribe-list nil
  "This list contained tag or username for auto subscribe")

(defvar juick-bookmarks '())

(defvar juick-api-aftermid nil)

(defvar juick-timer-interval 120)
(defvar juick-timer nil)

(defvar juick-tmp-dir
  (expand-file-name (concat "juick-images-" (user-login-name))
                    temporary-file-directory))

(if (not (file-directory-p juick-tmp-dir))
    (make-directory juick-tmp-dir))

;; from http://juick.com/help/
(defvar juick-id-regex "\\(#[0-9]+\\(/[0-9]+\\)?\\)")
(defvar juick-user-name-regex "[^0-9A-Za-z\\.]\\(@[0-9A-Za-z@\\.\\-]+\\)")
(defvar juick-tag-regex "\\(\\*[^ \n]+\\)")
(defvar juick-bold-regex "[\n ]\\(\\*[^\n]+*\\*\\)[\n ]")
(defvar juick-italic-regex "[\n ]\\(/[^\n]+/\\)[\n ]")
(defvar juick-underline-regex "[\n ]\\(\_[^\n]+\_\\)[\n ]")

(defvar juick-bookmark-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "q" 'juick-find-buffer)
    (define-key map (kbd "TAB") 'juick-next-button)
    (define-key map (kbd "<backtab>") 'backward-button)
    map)
  "Keymap for `juick-bookmark-mode'.")

(defun juick-markup-chat (from buffer text proposed-alert &optional force)
  "Markup  message from `juick-bot-jid'.

Where FROM is jid sender, BUFFER is buffer with message TEXT

Use FORCE to markup any buffer"
  (if (or force (string-match juick-bot-jid from))
      (save-excursion
        (set-buffer buffer)
        (when (null force)
          (jabber-truncate-top)
          (setq juick-point-last-message
                (re-search-backward (concat juick-bot-jid ">") nil t)))
        (when (> juick-point-last-message (point-max))
          (setq juick-point-last-message nil))
        (juick-markup-user-name)
        (juick-markup-id)
        (juick-markup-tag)
        (juick-markup-bold)
        (juick-markup-italic)
        (juick-markup-underline)
        (when (and juick-icon-mode window-system)
          (clear-image-cache)
          (juick-avatar-insert)))))

(add-hook 'jabber-alert-message-hooks 'juick-markup-chat)

(defun juick-avatar-insert ()
  (goto-char (or juick-point-last-message (point-min)))
  (setq juick-avatar-internal-stack nil)
  (let ((inhibit-read-only t))
    (while (re-search-forward "\\(by @\\|> @\\|^@\\)\\([0-9A-Za-z@\\.\\-]+\\):" nil t)
      (let* ((icon-string "\n ")
             (name (match-string-no-properties 2))
             (fake-png (concat juick-tmp-dir "/" name ".png")))
        (goto-char (match-beginning 0))
        (juick-avatar-download name)
        (set-text-properties
         1 2 `(display
               (image :type png
                      :file ,fake-png))
         icon-string)
        (re-search-forward "@" nil t)
        (goto-char (- (point) 1))
        (insert (concat icon-string " "))
        (re-search-forward ":" nil t)))))

(defun juick-avatar-download (name)
  "Download avatar from juick.com"
  (if (or (assoc-string name juick-avatar-internal-stack)
          (and (file-exists-p (concat juick-tmp-dir "/" name ".png"))
               (< (time-to-number-of-days
                   (time-subtract (current-time)
                                  (nth 5 (file-attributes (concat juick-tmp-dir "/" name ".png")))))
                  juick-avatar-update-day)))
      nil
    (let ((avatar-url (concat "http://juick.com/" name "/"))
          (url-request-method "GET"))
      (push name juick-avatar-internal-stack)
      (url-retrieve avatar-url
                    '(lambda (status name)
                       (let ((result-buffer (current-buffer)))
                         (goto-char (point-min))
                         (when (re-search-forward "http://i.juick.com/a/[0-9]+\.png" nil t)
                           (juick-avatar-download-and-save (match-string 0) name)
                           (kill-buffer result-buffer))))
                    (list name)))))

(defun juick-avatar-download-and-save (link name)
  "Extract image frim LINK and save it with NAME in
`juick-tmp-dir'"
  (let* ((filename (substring link (string-match "[0-9]+" link)))
         (avatar-url (concat "http://i.juick.com/" (if juick-icon-hight "a" "as") "/" filename))
         (url-request-method "GET"))
    (url-retrieve avatar-url
                  '(lambda (status name)
                     (let ((result-buffer (current-buffer))
                           (buffer-file-coding-system 'binary)
                           (file-coding-system 'binary)
                           (coding-system-for-write 'binary))
                       (delete-region (point-min) (re-search-forward "\n\n" nil t))
                       (write-region (point-min) (point-max) (concat juick-tmp-dir "/" name ".png"))
                       (kill-buffer (current-buffer))
                       (kill-buffer result-buffer)))
                  (list name))))

(defun juick-auto-update (&optional arg)
  (interactive "P")
  (let ((arg (if (numberp arg)
                 (prefix-numeric-value arg)
               1)))
    (cond
     ((and (> arg 0) (null juick-timer))
       (setq juick-timer
             (run-at-time "0 sec"
                          juick-timer-interval
                          #'juick-api-last-message))
       (message "auto update activated"))
     ((and (<= arg 0) juick-timer)
      (cancel-timer juick-timer)
      (setq juick-timer nil)
      (message "auto update deactivated")))))

(defun juick-api-request (juick-stanza type callback)
  "Make and process juick stanza
\(http://juick.com/help/api/xmpp/)"
  ;; XXX: get current jc or use specified jc?
  (unless (memq jabber-buffer-connection jabber-connections)
    (let ((new-jc (jabber-find-active-connection jabber-buffer-connection)))
      (if new-jc
          (setq jabber-buffer-connection new-jc)
        (setq jabber-buffer-connection (jabber-read-account)))))
  (jabber-send-iq jabber-buffer-connection juick-bot-jid type juick-stanza
                  callback nil
                  ;; this error code='404' (last message not found)
                  nil nil))

(defun juick-api-unsubscribe (id)
  "Unsubscribe to message with ID."
  (juick-api-request `(subscriptions ((xmlns . "http://juick.com/subscriptions#messages")
                              (action . "unsubscribe")
                              (mid . ,id))) "set" nil)
  (message "Unsubscribing to %s" id))

(defun juick-api-subscribe (id)
  "Subscribe to message with ID.

Recieving full new message."
  (juick-api-request `(subscriptions ((xmlns . "http://juick.com/subscriptions#messages")
                              (action . "subscribe")
                              (mid . ,id))) "set" nil)
  (message "Subscribing to %s" id))

(defun juick-api-last-message ()
  "Recieving last ten message after `juick-api-aftermid'"
  (juick-api-request `(query ((xmlns . "http://juick.com/query#messages")
                              ,(if juick-api-aftermid `(aftermid . ,juick-api-aftermid))))
                     "get" 'juick-api-last-message-cb))

(defun juick-api-last-message-cb (jc xml-data closure-data)
  "Checking `juick-auto-subscribe-list' and `juick-tag-subscribed'
in a match, if match send fake message himself"
  (let ((juick-query (jabber-xml-get-children
                      (car (jabber-xml-get-children xml-data 'query))
                      'juick))
        (first-message t)) ;; XXX: i dont know how break out of dolist
    (dolist (x juick-query)
      (if (> (string-to-number (jabber-xml-get-attribute x 'mid))
             (string-to-number (or juick-api-aftermid "0")))
          (setq juick-api-aftermid (jabber-xml-get-attribute x 'mid)))
      (setq first-message t)
      ;; Message with uname auto subscribe
      (when (assoc-string (jabber-xml-get-attribute x 'uname)
                          juick-auto-subscribe-list)
        (juick-api-subscribe (jabber-xml-get-attribute x 'mid)))
      (dolist (tag (jabber-xml-get-children x 'tag))
        ;; Message with tag auto subscribe
        (when (assoc-string (car (jabber-xml-node-children tag))
                            juick-auto-subscribe-list)
          (juick-api-subscribe (jabber-xml-get-attribute x 'mid)))
        (when (and first-message (assoc-string
                                  (car (jabber-xml-node-children tag))
                                  juick-tag-subscribed))
          ;; make fake incomning message
          (setq first-message nil)
          (jabber-process-chat
           (jabber-read-account)
           `(message
             ((from . ,juick-bot-jid))
             (body nil ,(concat
                         "@"
                         (jabber-xml-get-attribute x 'uname)
                         ": "
                         (mapconcat
                          (lambda (tag)
                            (concat "*" (car (jabber-xml-node-children tag))))
                          (jabber-xml-get-children x 'tag)
                          " ")
                         "\n"
                         (car (jabber-xml-node-children
                               (car (jabber-xml-get-children x 'body))))
                         "\n#" (jabber-xml-get-attribute x 'mid)
                         " (" (or (jabber-xml-get-attribute x 'replies) "0") " replies)"
                         " (S)")))))))))

(defun juick-bookmark-list ()
  (interactive)
  (let ((tmp-pos juick-point-last-message))
    (setq juick-point-last-message nil)
    (split-window-vertically -10)
    (windmove-down)
    (switch-to-buffer "*juick-bookmark*")
    (toggle-read-only -1)
    (delete-region (point-min) (point-max))
    (dolist (x juick-bookmarks)
      (insert (concat (car x) " " (cdr x) "\n")))
    (goto-char (point-min))
    (toggle-read-only)
    (juick-markup-chat juick-bot-jid (current-buffer) nil nil t)
    (setq juick-point-last-message tmp-pos)
    (juick-bookmark-mode)))

(defun juick-bookmark-add (id desc)
  (interactive)
  (when (not desc)
    (setq desc (read-string (concat "Type description for " id ": "))))
  (push `(,id . ,desc) juick-bookmarks))

(define-derived-mode juick-bookmark-mode text-mode
  "juick bookmark mode"
  "Major mode for getting bookmark")

(define-key jabber-chat-mode-map (kbd "TAB") 'juick-next-button)
(define-key jabber-chat-mode-map "\C-cjb" 'juick-bookmark-list)
(define-key jabber-chat-mode-map "b"
  '(lambda ()
     (interactive)
     (if (or (looking-at "#[0-9]+") (looking-at "@[0-9A-Za-z@\.\-]+"))
         (juick-bookmark-add (match-string 0) nil)
       (self-insert-command 1))))
(define-key jabber-chat-mode-map "s"
  '(lambda ()
     (interactive)
     (if (or (looking-at "#\\([0-9]+\\)") (looking-at "@[0-9A-Za-z@\.\-]+"))
         (if (match-string 1)
             (juick-api-subscribe (match-string-no-properties 1))
           (juick-send-message juick-bot-jid
                               (concat "S " (match-string-no-properties 0))))
       (self-insert-command 1))))
(define-key jabber-chat-mode-map "u"
  '(lambda ()
     (interactive)
     (if (or (looking-at "#\\([0-9]+\\)") (looking-at "@[0-9A-Za-z@\.\-]+"))
         (if (match-string 1)
             (juick-api-unsubscribe (match-string-no-properties 1))
           (juick-send-message juick-bot-jid
                               (concat "U " (match-string-no-properties 0))))
       (self-insert-command 1))))
(define-key jabber-chat-mode-map "d"
  '(lambda ()
     (interactive)
     (if (looking-at "#[0-9]+\\(/[0-9]+\\)?")
         (juick-send-message juick-bot-jid
                             (concat "D " (match-string-no-properties 0)))
       (self-insert-command 1))))
(define-key jabber-chat-mode-map "p"
  '(lambda ()
     (interactive)
     (if (looking-at "@[0-9A-Za-z@\.\-]+")
         (progn
           (goto-char (point-max))
           (delete-region jabber-point-insert (point-max))
           (insert (concat "PM " (match-string-no-properties 0) " ")))
       (self-insert-command 1))))

(defun juick-send-message (to text)
  "Send TEXT to TO imediately"
  (interactive)
  (save-excursion
    (let ((buffer (jabber-chat-create-buffer (jabber-read-account) to)))
      (set-buffer buffer)
      (goto-char (point-max))
      (delete-region jabber-point-insert (point-max))
      (insert text)
      (jabber-chat-buffer-send))))

(defun juick-markup-user-name ()
  "Markup user-name matched by regex `juick-regex-user-name'"
  (goto-char (or juick-point-last-message (point-min)))
  (while (re-search-forward juick-user-name-regex nil t)
    (when (match-string 1)
      (juick-add-overlay (match-beginning 1) (match-end 1)
                         'juick-user-name-face)
      (make-button (match-beginning 1) (match-end 1)
                   'action 'juick-insert-user-name))))

(defun juick-markup-id ()
  "Markup id matched by regex `juick-regex-id'"
  (goto-char (or juick-point-last-message (point-min)))
  (while (re-search-forward juick-id-regex nil t)
    (when (match-string 1)
      (juick-add-overlay (match-beginning 1) (match-end 1)
                         'juick-id-face)
      (make-button (match-beginning 1) (match-end 1)
                   'action 'juick-insert-id))))

(defun juick-markup-tag ()
  "Markup tag matched by regex `juick-regex-tag'"
  (goto-char (or juick-point-last-message (point-min)))
  ;;; FIXME: I dont know how to recognize a tag point
  (while (re-search-forward (concat juick-user-name-regex  "\: ") nil t)
    ;;(goto-char (+ (point) (length (match-string 1))))
    (let ((count-tag 0))
      (while (and (looking-at "\\*")
                  (<= count-tag 5))
        (let ((beg-tag (point))
              (end-tag (- (re-search-forward "[\n ]" nil t) 1)))
          (juick-add-overlay beg-tag end-tag 'juick-tag-face)
          (make-button beg-tag end-tag 'action 'juick-find-tag))
        (setq count-tag (+ count-tag 1))))))

(defun juick-markup-italic ()
  (goto-char (or juick-point-last-message (point-min)))
  (while (re-search-forward juick-italic-regex nil t)
    (juick-add-overlay (match-beginning 1) (match-end 1)
                       'juick-italic-face)
    (goto-char (- (point) 1))))

(defun juick-markup-bold ()
  (goto-char (or juick-point-last-message (point-min)))
  (while (re-search-forward juick-bold-regex nil t)
    (juick-add-overlay (match-beginning 1) (match-end 1)
                       'juick-bold-face)
    (goto-char (- (point) 1))))

(defun juick-markup-underline ()
  (goto-char (or juick-point-last-message (point-min)))
  (while (re-search-forward juick-underline-regex nil t)
    (juick-add-overlay (match-beginning 1) (match-end 1)
                       'juick-underline-face)
    (goto-char (- (point) 1))))

;;; XXX: maybe merge?
(defun juick-insert-user-name (button)
  "Inserting reply id in conversation buffer"
  (let ((user-name (buffer-substring-no-properties
                    (overlay-start button)
                    (- (re-search-forward "[\n :]" nil t) 1))))
    (when (string-match-p "*-jabber-chat-" (buffer-name))
      (message "Mark set")
      (push-mark))
    (juick-find-buffer)
    (goto-char (point-max))
    (insert (concat user-name " ")))
  (recenter 10))

(defun juick-insert-id (button)
  "Inserting reply id in conversation buffer"
  (let ((id (buffer-substring-no-properties
             (overlay-start button)
             (- (re-search-forward "[\n ]" nil t) 1))))
    (when (string-match-p "*-jabber-chat-" (buffer-name))
      (message "Mark set")
      (push-mark))
    (juick-find-buffer)
    (goto-char (point-max))
    ;; usually #NNNN supposed #NNNN+
    (if (string-match "/" id)
        (insert (concat id " "))
          (insert (concat id (if juick-reply-id-add-plus "+" " ")))))
  (recenter 10))

(defun juick-find-tag (button)
  "Retrive 10 message this tag"
  (save-excursion
    (let ((tag (buffer-substring-no-properties
                (overlay-start button)
                (re-search-forward "\\([\n ]\\|$\\)" nil t))))
      (juick-find-buffer)
      (delete-region jabber-point-insert (point-max))
      (goto-char (point-max))
      (insert tag)))
  (jabber-chat-buffer-send))

(defun juick-find-buffer ()
  "Find buffer with `juick-bot-jid'"
  (interactive)
  (when (not (string-match (concat "*-jabber-chat-" juick-bot-jid "-*")
                           (buffer-name)))
    (delete-window)
    (let ((juick-window (get-window-with-predicate
                         (lambda (w)
                           (string-match
                            (concat "*-jabber-chat-" juick-bot-jid "-*")
                            (buffer-name (window-buffer w)))))))
      (if juick-window
          (select-window juick-window)
        (jabber-chat-with (jabber-read-account) juick-bot-jid)))))

(defadvice jabber-chat-send (around jabber-chat-send-around-advice
                                    (jc body) activate)
  "Check and correct juick command"
  (if (string-match juick-bot-jid jabber-chatting-with)
      (let* ((body (cond
                    ((string= "№" body)
                     "#")
                    ((string= "РУДЗ" body)
                     "HELP")
                    ((string= "help" body)
                     "HELP")
                    ((string= "d l" body)
                     "D L")
                    (t
                     body))))
        ad-do-it)
    ad-do-it))

(defun juick-next-button ()
  "move point to next button"
  (interactive)
  (if (next-button (point))
      (goto-char (overlay-start (next-button (point))))
    (progn
      (goto-char (point-max))
      (message "button not found"))))

(defun juick-add-overlay (begin end faces)
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'face faces)
    (push overlay juick-overlays)))

(defun juick-delete-overlays ()
  (dolist (overlay juick-overlays)
    (delete-overlay overlay))
  (setq juick-overlays nil))

(provide 'juick)
;;; juick.el ends here
