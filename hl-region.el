;;; hl-region.el --- Passive reigon highlighting. -*- lexical-binding: t; -*-
;;
;; Copyright © 2020–present Walker Griggs <walker@walkergriggs.com>
;;
;; Author: Walker Griggs <walker@walkergriggs.com>
;; URL: https://github.com/walkergriggs/hl-region
;; Keywords: convenience
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Provides passive region highlighting so you'll never lose your place.

;;; Code:

(require 'hl-line) ;; hl-line-face

(defgroup hl-region nil
  "Highlight a region to earmark for later"
  :group  'text
  :tag    "Highlight Region"
  :prefix "hl-region-"
  :link   '(url-link :tag "GitHub" "https://github.com/walkergriggs/hl-region"))

(defcustom hl-region-sticky-flag t
  "Non-nil means the hl-region highlight appears in all windows.
Otherwise hl-region will highlight only in the selected
window."
  :type 'boolean
  :group 'hl-region)


;;;###autoload
(defun hl-region (beg end)
  "Highlights the active region."
  (interactive "r")
  (deactivate-mark)

  ;; Lazy -- todo: grow region when overlapping
  (dolist (overlay (overlays-in beg end))
    (when (hl-region--overlay-p overlay)
      (goto-char (overlay-start overlay))
      (error "Highlighted region already exists here")))

  (progn
    (setq earmark-overlay (hl-region--make-overlay beg end))
    (overlay-put earmark-overlay
                 'window (unless hl-region-sticky-flag (selected-window)))))

;;;###autoload
(defun hl-region-remove ()
  "Removes the highlighted region at the current point."
  (interactive)
  (mapc #'hl-region--remove (overlays-at (point))))

(defun hl-region--remove (overlay)
  "Removes the given overlay"
  (if (hl-region--overlay-p overlay)
      (delete-overlay overlay)))

(defun hl-region--make-overlay (beg end)
  "Creates a new overlay of type 'hl-region--region spanning the given region."
  (let ((overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay 'type 'hl-region--region)
    (overlay-put overlay 'priority -50)
    (overlay-put overlay 'face hl-line-face)
    overlay))

(defun hl-region--overlay-p (overlay)
  "Checks if overlay has been created by hl-region"
  (memq (overlay-get overlay 'type)
        '(hl-region--region)))

(provide 'hl-region)

;;; hl-region.el ends here
