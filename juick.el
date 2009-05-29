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
;; (define-key jabber-chat-mode-map (kbd "TAB") 'juick-next-button)

;;; Code:

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

;; from http://juick.com/help/
(defvar juick-id-regex "\\(#[0-9]+\\(/[0-9]+\\)?\\)")
(defvar juick-user-name-regex "[\n ]\\(@[0-9A-Za-z\\-]+\\)")
(defvar juick-tag-regex "\\:[\n]\\(\\*[^ \n$]+\\)")
(defvar juick-bold-regex "[\n ]\\(\\*.*\\*\\)[\n ]")
(defvar juick-italic-regex "[\n ]\\(/.*/\\)[\n ]")
(defvar juick-underline-regex "[\n ]\\(\_.*\_\\)[\n ]")

(defun juick-add-overlay (begin end faces)
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'face faces)
    (push overlay juick-overlays)))

(defun juick-delete-overlays ()
  (dolist (overlay juick-overlays)
    (delete-overlay overlay))
  (setq juick-overlays nil))

(defun jabber-message-juick (from buffer text proposed-alert &optional last)
  "Markup juick msg"
  (save-excursion
    (switch-to-buffer buffer)
    (setq startmsg (re-search-backward "juick@juick.com>" nil t))
    (if (or last startmsg)
        (while (re-search-forward
                (concat juick-id-regex "\\|"
                        juick-user-name-regex "\\|"
                        juick-tag-regex "\\|"
                        juick-bold-regex "\\|"
                        juick-italic-regex "\\|"
                        juick-underline-regex) nil t)
          (cond ((match-string 1) ;; #NNNNNN
                 (progn
                   (juick-add-overlay (match-beginning 1) (match-end 1)
                                      'juick-reply-id-face)
                   (make-button (match-beginning 1) (match-end 1)
                                'action 'juick-insert-reply-id)))
                ((match-string 3) ;; @user-name
                 (progn
                   (juick-add-overlay (match-beginning 3) (match-end 3)
                                      'juick-user-name-face)
                   (make-button (match-beginning 3) (match-end 3)
                                'action 'juick-insert-user-name)))
                ((match-string 4) ;; *tag
                 (progn
                   (juick-add-overlay (match-beginning 4) (match-end 4)
                                      'juick-tag-face)
                   (make-button (match-beginning 4) (match-end 4)
                                'action 'juick-find-tag)))
                ((match-string 5) ;; *bold*
                 (progn
                   (juick-add-overlay (match-beginning 5) (match-end 5)
                                      'juick-bold-face)
                   (goto-char (- (point) 1)))) ;; next ' ' or '\n'
                ((match-string 6) ;; /italic/
                 (progn
                   (juick-add-overlay (match-beginning 6) (match-end 6)
                                      'juick-italic-face)
                   (goto-char (- (point) 1)))) ;; next ' ' or '\n'
                ((match-string 7) ;; _underline_
                 (progn
                   (juick-add-overlay (match-beginning 7) (match-end 7)
                                      'juick-underline-face)
                   (goto-char (- (point) 1))))))))) ;; next ' ' or '\n'

(defun juick-insert-reply-id (button)
  "Inserting reply id"
  (save-excursion
    (goto-char (overlay-start button))
    (re-search-forward juick-id-regex nil t)
    (setq id (match-string 1))
    (set-text-properties 0 (length id) nil id)
    (juick-find-buffer)
    (goto-char (point-max))
    (insert (concat id " ")))
  (goto-char (point-max))
  (recenter 10))

(defun juick-insert-user-name (button)
  "Inserting reply id"
  (save-excursion
    (goto-char (- (overlay-start button) 1)) ;; begining ' ' or '\n'
    (re-search-forward juick-user-name-regex nil t)
    (setq id (match-string 1))
    (set-text-properties 0 (length id) nil id)
    (juick-find-buffer)
    (goto-char (point-max))
    (insert (concat id " ")))
  (goto-char (point-max))
  (recenter 10))

(defun juick-find-tag (button)
  "Retrive 10 msg this tag"
  (save-excursion
    (goto-char (- (overlay-start button) 2)) ;; begining ':\n'
    (re-search-forward juick-tag-regex nil t)
    (setq tag (match-string 0))
    (set-text-properties 0 (length tag) nil tag)
    (juick-find-buffer)
    (delete-region jabber-point-insert (point-max)))
  (goto-char (point-max))
  (insert tag)
  (jabber-chat-buffer-send))

(defun juick-find-buffer ()
  (interactive)
  (if (not (string-match "*-jabber-chat-juick@juick.com-*" (buffer-name)))
      (progn
        (delete-window)
        (let ((juick-window (get-window-with-predicate
                             (lambda (w)
                               (string-match
                                "*-jabber-chat-juick@juick.com-*"
                                (buffer-name (window-buffer w)))))))
          (if juick-window
              (select-window juick-window)
            (jabber-chat-with (jabber-read-account) "juick@juick.com"))))))

;;;
;; (setq jabber-history-enabled t)
;; (setq jabber-use-global-history nil)
(defun juick-last-reply ()
  "Retrive last reply"
  (interactive)
  (split-window-vertically -10)
  (windmove-down)
  (switch-to-buffer "*juick-last-reply*")
  (toggle-read-only -1)
  (delete-region (point-min) (point-max))
  (setq list (jabber-history-query nil nil 10 "out" "juick@juick.com"
                                (concat jabber-history-dir "/juick@juick.com")))
  (while list
    (let ((msg (aref (car list) 4)))
      (if (string-match "\\(^#[0-9]+\\(/[0-9]+\\)?\\)" msg 0)
          (progn
            (if ( > (length msg) 20)
                (insert (substring msg 0 20))
              (insert msg))
            (insert "\n"))))
    (setq list (cdr list)))
  (goto-char (point-min))
  (local-set-key "q" 'juick-find-buffer)
  (local-set-key (kbd "TAB") 'juick-next-button)
  (local-set-key (kbd "<backtab>") 'backward-button)
  (toggle-read-only)
  (jabber-message-juick nil (current-buffer) nil nil t))

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
;;          (with-temp-buffer
;;            (insert body)
;;            (goto-char (point-min))
;;            (tiny-url-replace)
;;            (buffer-string))))
;;     (jabber-chat-send jc newbody))
;;     (jabber-chat-send jc body)))

(provide 'juick)
;;; juick.el ends here
