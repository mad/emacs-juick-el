;;; jabber-moods.el --- XEP-0107

;; Copyright (C) 2009  mad

;; Author: mad <owner.mad.epa@gmail.com>
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

;;

;;; Code:

(defvar jabber-mood-values
  '("afraid" "amazed" "amorous" "angry" "annoyed" "anxious" "aroused"
    "ashamed" "bored" "brave" "calm" "cautious" "cold" "confident"
    "confused" "contemplative" "contented" "cranky" "crazy" "creative"
    "curious" "dejected" "depressed" "disappointed" "disgusted"
    "dismayed" "distracted" "embarrassed" "envious" "excited"
    "flirtatious" "frustrated" "grateful" "grieving" "grumpy" "guilty"
    "happy" "hopeful" "hot" "humbled" "humiliated" "hungry" "hurt"
    "impressed" "in_awe" "in_love" "indignant" "interested"
    "intoxicated" "invincible" "jealous" "lonely" "lost" "lucky"
    "mean" "moody" "nervous" "neutral" "offended" "outraged" "playful"
    "proud" "relaxed" "relieved" "remorseful" "restless" "sad"
    "sarcastic" "satisfied" "serious" "shocked" "shy" "sick" "sleepy"
    "spontaneous" "stressed" "strong" "surprised" "thankful" "thirsty"
    "tired" "undefined" "unknown" "weak" "worried"))

(defvar jabber-mood-current nil)

(defmacro jabber-mood-menu ()
  (declare (indent 1))
  (define-key global-map [menu-bar jabber-menu mood]
    (cons "Mood" (make-sparse-keymap "Mood")))
  (cons 'progn
        (mapcar (lambda (mood)
                  `(define-key global-map [menu-bar jabber-menu mood ,(intern mood)]
                     '(menu-item ,mood
                                 (lambda ()
                                   (interactive)
                                   (setq jabber-mood-current ,mood))
                                 :button (:radio . (string= jabber-mood-current ,mood)))))
                jabber-mood-values)))

(jabber-mood-menu)

(defun jabber-mood-message ()
  (interactive)
  (add-hook 'jabber-chat-send-hooks 'jabber-mood-make-stanza)
  (setq jabber-mood-current (completing-read (concat "Set your mood (\"" jabber-mood-current "\"): ")
                                             jabber-mood-values nil t nil nil
                                             jabber-mood-current)))

(defun jabber-mood-make-stanza (body id)
  (when (member jabber-mood-current jabber-mood-values)
    (remove-hook 'jabber-chat-send-hooks 'jabber-mood-make-stanza)
    `((mood ((xmlns . "http://jabber.org/protocol/mood"))
            (,(intern jabber-mood-current))))))

(provide 'jabber-moods)
;;; jabber-moods.el ends here
