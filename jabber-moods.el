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
  '("afraid"
    "amazed"
    "amorous"
    "angry"
    "annoyed"
    "anxious"
    "aroused"
    "ashamed"
    "bored"
    "brave"
    "calm"
    "cautious"
    "cold"
    "confident"
    "confused"
    "contemplative"
    "contented"
    "cranky"
    "crazy"
    "creative"
    "curious"
    "dejected"
    "depressed"
    "disappointed"
    "disgusted"
    "dismayed"
    "distracted"
    "embarrassed"
    "envious"
    "excited"
    "flirtatious"
    "frustrated"
    "grateful"
    "grieving"
    "grumpy"
    "guilty"
    "happy"
    "hopeful"
    "hot"
    "humbled"
    "humiliated"
    "hungry"
    "hurt"
    "impressed"
    "in_awe"
    "in_love"
    "indignant"
    "interested"
    "intoxicated"
    "invincible"
    "jealous"
    "lonely"
    "lost"
    "lucky"
    "mean"
    "moody"
    "nervous"
    "neutral"
    "offended"
    "outraged"
    "playful"
    "proud"
    "relaxed"
    "relieved"
    "remorseful"
    "restless"
    "sad"
    "sarcastic"
    "satisfied"
    "serious"
    "shocked"
    "shy"
    "sick"
    "sleepy"
    "spontaneous"
    "stressed"
    "strong"
    "surprised"
    "thankful"
    "thirsty"
    "tired"
    "undefined"
    "unknown"
    "weak"
    "worried"))

(defvar jabber-mood-current nil)

(defun jabber-mood-message ()
  (interactive)
  (add-hook 'jabber-chat-send-hooks 'jabber-mood-make-stanza)
  (let ((mood (completing-read "Select moods: " jabber-mood-values nil t)))
    (setq jabber-mood-current mood)))

(defun jabber-mood-make-stanza (body id)
  (when jabber-mood-current
    (remove-hook 'jabber-chat-send-hooks 'jabber-mood-make-stanza)
    `((mood ((xmlns . "http://jabber.org/protocol/mood"))
            (,(intern jabber-mood-current))))))

(provide 'jabber-moods)
;;; jabber-moods.el ends here
