;;; google-maps.el --- used to retrieve location coordinates

;; Copyright (C) 2009  mad

;; Author: mad <owner.mad.epa@gmail.com>
;; Keywords:

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

(require 'json)

(defun google-map-get-location (location)
  "Get coordinates location LOCATION from google maps
\(Geocoding via HTTP\
\"http://code.google.com/apis/maps/documentation/services.html#Geocoding_Direct\" \)

Return array with lat and lon (eg [30.333 59.3333])"
  (let* ((request location)
         (google-url (concat
                      "http://maps.google.com/maps/geo?q="
                      (url-hexify-string request)
                      "&output=json&oe=utf8&sensor=true_or_false&key=emacs-jabber"))
         (url-request-method "GET")
         (content-buf (url-retrieve-synchronously google-url)))
    (save-excursion
      (set-buffer content-buf)
      (goto-char (point-min))
      (delete-region (point-min) (re-search-forward "\n\n" nil t))
      (let ((maybe-loc (json-read)))
        (kill-buffer (current-buffer))
        (if (= (length maybe-loc) 3)
            (progn
              (cdar (cdar (aref (cdr (car maybe-loc)) 0))))
          nil)))))

(provide 'google-maps)
;;; google-maps.el ends here
