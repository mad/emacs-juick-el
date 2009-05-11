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

;;; Installing:

;; (require 'juick)
;; (add-hook 'jabber-alert-message-hooks 'jabber-message-juick)

;;; for better movement through id (#XXXXXX) and user name (@USER)

;; (define-key jabber-chat-mode-map (kbd "TAB") 'juick-next-button)

;;; for teplace long link

;; (define-key jabber-chat-mode-map "\C-ct" '(lambda()
;; 					    (interactive)
;; 					    (save-excursion
;; 					      (tiny-url-replace jabber-point-insert))))

;;; Code:

(require 'jabber)
(require 'button)

(defgroup juick-faces nil "Faces for displaying Juick msg"
  :group 'juick)

(defface juick-reply-id-face
  '((t (:weight bold)))
  "face for displaying id"
  :group 'juick-faces)

(defface juick-user-name-face
  '((t (:foreground "blue" :weight bold :slant normal)))
  "face for displaying user name"
  :group 'juick-faces)

(defface juick-tag-face
  '((t (:foreground "black" :background "light gray" :slant italic)))
  "face for displaying tags"
  :group 'juick-faces)

(defvar juick-overlays nil)

(defun juick-add-overlay (begin end faces)
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'face faces)
    (push overlay juick-overlays)))

(defun juick-delete-overlays ()
  (dolist (overlay juick-overlays)
    (delete-overlay overlay))
  (setq juick-overlays nil))

(defun jabber-message-juick (from buffer text proposed-alert)
  "Markup juick msg"
  (switch-to-buffer buffer)
  (save-excursion
    (setq startmsg (re-search-backward "juick@juick.com>" nil t))
    (if startmsg
	(while (re-search-forward
	 	"\\(#[0-9]+\\(/[0-9]+\\)?\\)\\|[\t\n ]+\\(@[0-9A-Za-z\-]+\\)\\|\\:[\n]\\(\\*[^ \n\t$]+\\)" nil t)
	  (if (match-string 1) ;; #NNNNNN
	      (progn
		(juick-add-overlay (match-beginning 1) (match-end 1)
				   'juick-reply-id-face)
		(make-button (match-beginning 1) (match-end 1)
			     'action 'juick-insert-reply-id)))
	  (if (match-string 3) ;; @user-name
	      (progn
		(juick-add-overlay (match-beginning 3) (match-end 3)
				   'juick-user-name-face)
		(make-button (match-beginning 3) (match-end 3)
			     'action 'juick-insert-user-name)))
 	  (if (match-string 4) ;; *tag
	      (progn
		(juick-add-overlay (match-beginning 4) (match-end 4)
				   'juick-tag-face)
		(make-button (match-beginning 4) (match-end 4)
			     'action 'juick-find-tag)))))))

(defun juick-insert-reply-id (button)
  "Inserting reply id"
  (save-excursion
    (goto-char (overlay-start button))
    (re-search-forward "\\(#[0-9]+\\(/[0-9]+\\)?\\)" nil t)
    (goto-char (point-max))
    (setq id (match-string 0))
    (set-text-properties 0 (length id) nil id)
    (insert (concat id " ")))
  (goto-char (point-max))
  (recenter 10))

(defun juick-insert-user-name (button)
  "Inserting reply id"
  (save-excursion
    (goto-char (overlay-start button))
    (re-search-forward "\\(@[0-9A-Za-z\-]+\\)" nil t)
    (goto-char (point-max))
    (setq id (match-string 0))
    (set-text-properties 0 (length id) nil id)
    (insert (concat id " ")))
  (goto-char (point-max))
  (recenter 10))

(defun juick-find-tag (button)
  "retrive 10 msg this tag"
  (save-excursion
    (goto-char (overlay-start button))
    (re-search-forward "\\(\\*[^ \n\t$]+\\)" nil t)
    (setq tag (match-string 0))
    (set-text-properties 0 (length tag) nil tag)
    (delete-region jabber-point-insert (point-max)))
  (goto-char (point-max))
  (insert (concat tag ))
  (jabber-chat-buffer-send))

(defun juick-next-button ()
  "move point to next button"
  (interactive)
  (if (next-button (point))
      (goto-char (overlay-start (next-button (point))))
    (progn
      (goto-char (point-max))
      (message "button not found"))))

;;

(defun tiny-url-replace(&optional pos)
  "Replace normal url to tinyurl.com in the
current buffer.

If POS set to find url from this POS or
`point-min'"
  (interactive)
  (goto-char (or pos (point-min)))
  (while (re-search-forward "http://" nil t)
    (goto-char (match-beginning 0))
    (let* ((url-bounds (bounds-of-thing-at-point 'url))
	   (url (thing-at-point 'url))
	   (newurl
	    (save-excursion
	      (set-buffer
	       (url-retrieve-synchronously
		(concat "http://tinyurl.com/api-create.php?url=" url)))
	      (goto-char (point-min))
	      (re-search-forward "http://tinyurl.com/.*" nil t)
	      (setq res (match-string 0))
	      (kill-buffer) res)))
      (save-restriction
	(narrow-to-region (car url-bounds) (cdr url-bounds))
	(delete-region (point-min) (point-max))
	(insert newurl)))))

;;; XXX: this doesn't work properly
;; (defun jabber-chat-send-replace-url (jc body)
;;   "Replace normal url to tinyurl.com/XXXX."
;;   (message "hi %s" jabber-chatting-with)
;;   (if (string-match "juick@juick.com" jabber-chatting-with)
;;       (let ((newbody
;; 	     (with-temp-buffer
;; 	       (insert body)
;; 	       (goto-char (point-min))
;; 	       (tiny-url-replace)
;; 	       (buffer-string))))
;; 	(jabber-chat-send jc newbody))
;;     (jabber-chat-send jc body)))

(provide 'juick)
;;; juick.el ends here
