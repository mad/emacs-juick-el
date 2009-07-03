;;; jabber-geoloc.el --- XEP-0080 implementation

;; Copyright (C) 2009  mad

;; Author:  mad <owner.mad.epa@gmail.com>
;; Keywords: jabber

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

;; You can add the following line to your init.el:

;; (define-key jabber-chat-mode-map "\C-cjg" 'jabber-chat-send-with-location)
;; (define-key jabber-chat-mode-map "\C-cjp" 'jabber-pep-location-send)


;;

;;; Code:

(defun jabber-chat-send-add-geoloc (body id)
  "Add geoloc to message"
  (jabber-geoloc-make-stanza))

(defun jabber-chat-send-with-location ()
  "Wrapper for add `jabber-chat-send-add-geoloc'"
  (interactive)
  (add-hook 'jabber-chat-send-hooks 'jabber-chat-send-add-geoloc)
  (jabber-chat-buffer-send)
  (remove-hook 'jabber-chat-send-hooks 'jabber-chat-send-add-geoloc))

(defun jabber-geoloc-make-stanza ()
  (let* ((maybe-loc (read-string "Input your location: "))
         (maybe-loc (google-map-get-location maybe-loc)))
    (if maybe-loc
        (let ((loc-stanza
               `((geoloc ((xmlns . "http://jabber.org/protocol/geoloc"))
                         (lat nil ,(prin1-to-string (aref maybe-loc 1)))
                         (lon nil ,(prin1-to-string (aref maybe-loc 0)))))))
          loc-stanza)
      (if (y-or-n-p (concat "Your location not found. Send without loc? "))
          nil
        (jabber-geoloc-make-stanza)))))

(defun jabber-pep-location-send ()
  "Send PEP with your location

Not work on many jabber servers"
  (interactive)
  (unless (memq jabber-buffer-connection jabber-connections)
    (let ((new-jc (jabber-find-active-connection jabber-buffer-connection)))
      (if new-jc
          (setq jabber-buffer-connection new-jc)
        (setq jabber-buffer-connection (jabber-read-account t)))))
  (let* ((id (apply 'format "emacs-msg-%d.%d.%d" (current-time)))
         (pep (jabber-geoloc-make-stanza))
         (stanza-to-send `(iq
                           ((from . ,(jabber-connection-bare-jid jabber-buffer-connection))
                            (id . ,id)
                            (type . "set"))
                           (pubsub ((xmlns . "http://jabber.org/protocol/pubsub"))
                                   (publish ((node . "http://jabber.org/protocol/geoloc"))
                                            (item nil
                                                  ,pep))))))
    (jabber-send-sexp jabber-buffer-connection stanza-to-send)))

(provide 'jabber-geoloc)
;;; jabber-geoloc.el ends here
