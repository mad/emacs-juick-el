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

;; (setq juick-icon-mode t)
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

(defvar juick-overlays nil)

(defvar juick-bot-jid "juick@juick.com")

(defvar juick-image-buffer "*juick-avatar-dir*")

(defvar juick-point-last-message nil)

(defvar juick-icon-mode nil
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
        (when (null force)
          (jabber-truncate-top)
          (setq juick-point-last-message
                (search-backward (concat juick-bot-jid ">") nil t)))
        (set-buffer buffer)
        (juick-markup-user-name)
        (juick-markup-id)
        (juick-markup-tag)
        (juick-markup-bold)
        (juick-markup-italic)
        (juick-markup-underline))))

(add-hook 'jabber-alert-message-hooks 'juick-markup-chat)

(defadvice jabber-chat-normal-body (around jabber-chat-normal-body-around-advice
                                           (xml-data who mode) activate)
  "Check xml-data, if xmlns exists and equal juick.com
print it, otherwise `ad-do-it'"
  (let ((juick-body (car (jabber-xml-get-children xml-data 'juick))))
    (if juick-body
        ;; Handle juick message
        (progn
          (setq ad-return-value t)
          (when (eql mode :insert)
            (setq juick-point-last-message (point))
            (insert (juick-api-make-message juick-body t))))
      ad-do-it)))

;; API funcs

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

(defun juick-api-message (id)
  "Retrieve only first message (without replies)"
  (juick-api-request `(query ((xmlns . "http://juick.com/query#messages")
                              (mid . ,id)))
                     "get" 'juick-api-message-cb))

(defun juick-api-message-cb (jc xml-data closure-data)
  (let* ((juick-query
          (jabber-xml-get-children
           (car (jabber-xml-get-children xml-data 'query))
           'juick))
         (fake-body
          (mapconcat
           (lambda (x)
             (juick-api-make-message x t))
           juick-query "\n\n")))
    (jabber-process-chat jc `(message
                              ((from . ,juick-bot-jid))
                              (body nil ,fake-body)))))

(defun juick-api-message-and-replies (id)
  "Retrieve full (with all replies) message with ID"
  (juick-api-message id) ;; first retrieve main message
  (juick-api-request `(query ((xmlns . "http://juick.com/query#messages")
                              (mid . ,id)
                              (rid . "*")))
                     "get"
                     'juick-api-message-and-replies-cb))

(defun juick-api-message-and-replies-cb (jc xml-data closure-data)
  (let* ((juick-query (jabber-xml-get-children
                       (car (jabber-xml-get-children xml-data 'query))
                       'juick))
         (fake-body
          (mapconcat
           (lambda (x)
             (juick-api-make-message x nil))
           juick-query "\n\n")))
    (jabber-process-chat jc `(message
                              ((from . ,juick-bot-jid))
                              (body nil ,fake-body)))))

(defun juick-api-last-ten-message ()
  "Recieving last ten messages"
  (juick-api-request `(query ((xmlns . "http://juick.com/query#messages")))
                      "get" 'juick-api-last-ten-message-cb))

(defun juick-api-last-ten-message-cb (jc xml-data closure-data)
  "Recieving ten messages from juick and sending himself (fake)"
  (let* ((juick-query (jabber-xml-get-children
                       (car (jabber-xml-get-children xml-data 'query))
                       'juick))
         (fake-body
          (mapconcat
           (lambda (x)
             (juick-api-make-message x t))
           juick-query "\n\n")))
    (jabber-process-chat jc `(message
                              ((from . ,juick-bot-jid))
                              (body nil ,fake-body)))))

(defun juick-api-last-message ()
  "Recieving last messages after `juick-api-aftermid'

Used for autoupdate"
  (juick-api-request `(query ((xmlns . "http://juick.com/query#messages")
                              ,(if juick-api-aftermid `(aftermid . ,juick-api-aftermid))))
                     "get" 'juick-api-last-message-cb))

(defun juick-api-last-message-cb (jc xml-data closure-data)
  "Checking `juick-auto-subscribe-list' and `juick-tag-subscribed', if
match send fake message himself"
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
          (jabber-process-chat (jabber-read-account)
                               `(message
                                 ((from . ,juick-bot-jid))
                                 (body nil ,(juick-api-make-message x t)))))))))

(defun juick-api-make-message (body-xml normal-body)
  "Make formatting message from body

if NORMAL-BODY t then BODY-XML without reply"
  (let* ((icon-string "\n  ")
         (avatar-id (jabber-xml-get-attribute body-xml 'uid))
         (replies (string-to-number (or (jabber-xml-get-attribute body-xml 'replies) "0")))
         (rid (jabber-xml-get-attribute body-xml 'rid))
         (mid (jabber-xml-get-attribute body-xml 'mid))
         (uname (jabber-xml-get-attribute body-xml 'uname))
         (body (car (jabber-xml-node-children
                     (car (jabber-xml-get-children body-xml 'body)))))
         (fake-png (concat juick-tmp-dir "/" avatar-id ".png")))
    ;; XXX: check timestamp, because avatar may be changed
    (when (not (file-exists-p (concat juick-tmp-dir "/" avatar-id ".png")))
      (write-region "" nil fake-png nil nil nil nil)
      (juick-avatar-download avatar-id))
    (set-text-properties
     1 2 `(display
           (image :type png
                  :file ,fake-png))
     icon-string)
    ;; XXX: add quoted message (lack of api)
    (concat (if (and rid normal-body)
                "Reply by ")
            (if (not mid) ;; is private message
                "Private message from ")
            (if juick-icon-mode
                icon-string)
            "@" uname ": "
            (mapconcat
             (lambda (tag)
               (concat "*" (car (jabber-xml-node-children tag))))
             (jabber-xml-get-children body-xml 'tag)
             " ")
            "\n"
            body
            "\n"
            (when mid ;; otherwise private message
              (concat "#" mid
                      (if rid
                          (concat "/" rid)
                        (cond
                         ((= 1 replies)
                          (concat " (" (number-to-string replies) " reply)"))
                         ((< 1 replies)
                          (concat " (" (number-to-string replies) " replies)"))))
                      (when normal-body
                        (concat " http://juick.com/" mid
                                (if rid
                                    (concat "#" rid)))))))))

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

(defun juick-auto-update (&optional arg)
  "Check last messages to match with `juick-tag-subscribed'
`juick-auto-subscribe-list'"
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

;; Keybindings
;; XXX: own minor mode ?

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

;; Markup funcs

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
    (juick-find-buffer)
    (goto-char (point-max))
    (insert (concat user-name " ")))
  (recenter 10))

(defun juick-insert-id (button)
  "Inserting reply id in conversation buffer"
  (let ((id (buffer-substring-no-properties
             (overlay-start button)
             (- (re-search-forward "[\n ]" nil t) 1))))
    (juick-find-buffer)
    (goto-char (point-max))
    ;; usually used #NNNN+ instead #NNNN
    (if (string-match "/" id)
        (insert (concat id " "))
      (insert (concat id "+"))))
  (recenter 10))

;; Other funcs

;; TODO: save bookmark and update reply
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

(defun juick-avatar-download (id)
  "Download avatar from juick.com"
  (let ((avatar-url
         (concat "http://i.juick.com/" (if juick-icon-hight "a" "as") "/" id ".png"))
        (url-request-method "GET"))
    (url-retrieve avatar-url
                  '(lambda (status id)
                     (let ((result-buffer (current-buffer))
                           (buffer-file-coding-system 'binary)
                           (file-coding-system 'binary)
                           (coding-system-for-write 'binary))
                       (delete-region (point-min) (re-search-forward "\n\n" nil t))
                       (write-region (point-min) (point-max) (concat juick-tmp-dir "/" id ".png"))
                       (kill-buffer (current-buffer))
                       (kill-buffer result-buffer)))
                  (list id))))

(defadvice jabber-chat-send (around jabber-chat-send-around-advice
                                    (jc body) activate)
  "Check and correct juick command"
  (if (string-match juick-bot-jid jabber-chatting-with)
      (let* ((body (cond
                    ((member-ignore-case  body '("№" "#" "LAST" "ДФЫЕ"))
                     (juick-api-last-ten-message)
                     ;; do not send original message
                     nil)
                    ((string-match "^#\\([0-9]+\\(/[0-9]+\\)?\\)$" body)
                     (juick-api-message (match-string 1 body))
                     nil)
                    ((string-match "^#\\([0-9]+\\(/[0-9]+\\)?\\)\\+$" body)
                     (juick-api-message-and-replies (match-string 1 body))
                     nil)
                    ((member-ignore-case body '("HELP" "РУДЗ"))
                     "HELP")
                    ((member-ignore-case body '("D L" "В Д"))
                     "D L")
                    (t
                     body))))
        (if body
            ad-do-it))
    ad-do-it))

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
