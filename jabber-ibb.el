;;; jabber-ibb.el --- ibb XEP-0047

;; Copyright (C) 2009 mad

;; Author:  mad <owner.mad.epa@gmail.com>
;; Keywords: jabber, ibb

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

;; Its workaround for send image to juick.
;; Its works ONLY one side.

;;; Code:

(defvar jabber-ibb-blocksize 4096
  "XEP-0047 RECOMMENDED to use a 'block-size' of 4096.")

(defvar jabber-ibb-jc nil)
(defvar jabber-ibb-jid nil)
(defvar jabber-ibb-sid nil)

(add-to-list 'jabber-si-stream-methods
             (list "http://jabber.org/protocol/ibb"
                   'jabber-ibb))

(defun jabber-ibb-send (data sequence)
  "Sending data in an IQ stanza

<iq from='romeo@montague.net/orchard'
    id='kr91n475'
    to='juliet@capulet.com/balcony'
    type='set'>
  <data xmlns='http://jabber.org/protocol/ibb' seq='0' sid='i781hf64'>
    qANQR1DBwU4DX7jmYZnncmUQB/9KuKBddzQH+tZ1ZywKK0yHKnq57kWq+RFtQdCJ
    WpdWpR0uQsuJe7+vh3NWn59/gTc5MDlX8dS9p0ovStmNcyLhxVgmqS8ZKhsblVeu
    IpQ0JgavABqibJolc3BKrVtVV1igKiX/N7Pi8RtY1K18toaMDhdEfhBRzO/XB0+P
    AQhYlRjNacGcslkhXqNjK5Va4tuOAPy2n1Q8UUrHbUd0g+xJ9Bm0G0LZXyvCWyKH
    kuNEHFQiLuCY6Iv0myq6iX6tjuHehZlFSh80b5BVV9tNLwNR5Eqz1klxMhoghJOA
  </data>
</iq>"
  (let* ((id (apply 'format "emacs-msg-%d.%d.%d" (current-time)))
         (stanza-to-send `(iq
                           ((from . ,(jabber-connection-bare-jid jabber-ibb-jc))
                            (to . ,jabber-ibb-jid)
                            (id . ,id)
                            (type . "set"))
                           (data ((xmlns . "http://jabber.org/protocol/ibb")
                                  (seq . ,sequence)
                                  (sid . ,jabber-ibb-sid))
                                 ,data))))
    (jabber-send-sexp jabber-ibb-jc stanza-to-send)))

(defun jabber-ibb-open ()
  "Initiate requests session

<iq from='romeo@montague.net/orchard'
    id='jn3h8g65'
    to='juliet@capulet.com/balcony'
    type='set'>
  <open xmlns='http://jabber.org/protocol/ibb'
        block-size='4096'
        sid='i781hf64'
        stanza='iq'/>
</iq>"
  (let* ((id (apply 'format "emacs-msg-%d.%d.%d" (current-time)))
         (stanza-to-send `(iq
                           ((from . ,(jabber-connection-bare-jid jabber-ibb-jc))
                            (to . ,jabber-ibb-jid)
                            (id . ,id)
                            (type . "set"))
                           (open ((xmlns . "http://jabber.org/protocol/ibb")
                                  (block-size . ,jabber-ibb-blocksize)
                                  (sid . ,jabber-ibb-sid)
                                  (stanza . "iq"))))))
    (jabber-send-sexp jabber-ibb-jc stanza-to-send)))

(defun jabber-ibb-close ()
  "Closing the bytestream

<iq from='romeo@montague.net/orchard'
    id='us71g45j'
    to='juliet@capulet.com/balcony'
    type='set'>
  <close xmlns='http://jabber.org/protocol/ibb' sid='i781hf64'/>
</iq>"
  (let* ((id (apply 'format "emacs-msg-%d.%d.%d" (current-time)))
         (stanza-to-send `(iq
                           ((from . ,(jabber-connection-bare-jid jabber-ibb-jc))
                            (to . ,jabber-ibb-jid)
                            (id . ,id)
                            (type . "set"))
                           (close ((xmlns . "http://jabber.org/protocol/ibb")
                                   (sid . ,jabber-ibb-sid))))))
    (jabber-send-sexp jabber-ibb-jc stanza-to-send)))

(defun jabber-ibb-send-split (data)
  "Split DATA by `jabber-ibb-blocksize' byte and send it"
  (with-temp-buffer
    (insert data)
    (base64-encode-region (point-min) (point-max))
    (goto-char (point-min))
    (let ((sequence 0))
      (while (> (- (point-max) (point)) jabber-ibb-blocksize)
        (jabber-ibb-send (buffer-substring (point) (+ (point) jabber-ibb-blocksize))
                         sequence)
        (setq sequence (+ sequence 1))
        (goto-char (+ (point) jabber-ibb-blocksize))))
    (if (not (= (point) (point-max)))
        (jabber-ibb-send (buffer-substring (point) (point-max)) sequence))
    (jabber-ibb-close)))

(defun jabber-ibb (jc jid sid profile-function)
  (setq jabber-ibb-jc jc)
  (setq jabber-ibb-jid jid)
  (setq jabber-ibb-sid sid)
  (jabber-ibb-open)
  (funcall profile-function jc jid sid
           (lambda (data)
             (jabber-ibb-send-split data))))

(provide 'jabber-ibb)
;;; jabber-ibb.el ends here
