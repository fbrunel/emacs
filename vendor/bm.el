;;; bm.el  -- Visible bookmarks in buffer.

;; Copyrigth (C) 2000, 2001  Jo Odland

;; Author: Jo Odland <jood@online.no>
;; Time-stamp:	<Wed Jan 31 17:34:28 2001  jood>
;; Version: 0.5
;; Keywords; visible bookmarks, overlay
;; URL: http://home.online.no/~jood/emacs/bm.el

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Description:
;;
;;   This package was created because I missed the
;;   bookmarks from M$ Visual Studio. They provide
;;   an easy way to navigate in a buffer.
;;
;;   bm.el provides visible, buffer local, bookmarks and the
;;   ability to jump forward and backward to the next bookmark.
;;
;;   The use of overlays for bookmarks was inspired by highline.el
;;   by Vinicius Jose Latorre <vinicius@cpqd.com.br>.


;; Installation:
;;
;;   To use bm.el, put it in your load-path and add
;;   the following to your .emacs
;;
;;   (require 'bm)
;;
;;   This package is developed and tested on 
;;   GNU Emacs 20.4.1 on RedHat Linux 6.2. 


;; Configuration:
;;
;;   Available commands:
;;     bm-toggle-bookmark
;;     bm-goto-bookmark
;;     bm-goto-bookmark-previous
;;     bm-toggle-wrapping
;;     bm-delete-all
;;
;;   To make it easier to use, assign the commands to some keys.
;;
;;   M$ Visual Studio key setup.
;;     (global-set-key [C-f2] 'bm-toggle-bookmark)
;;     (global-set-key [f2] 'bm-goto-bookmark)
;;     (global-set-key [S-f2] 'bm-goto-bookmark-previous)
;;  
;;

;; Todo:
;;   


;;; Code
;;

(defgroup bm nil
  "Toggle visible, buffer local, bookmarks."
  :link '(emacs-library-link :tag "Source Lisp File" "bm.el")
  :group 'faces
  :group 'editing
  :prefix "bm-")


(defcustom bm-face 'bm-face
  "*Specify face used to highlight the current line."
  :type 'face
  :group 'bm)


(defcustom bm-priority 0
  "*Specify bm overlay priority.

Higher integer means higher priority, so bm overlay will have precedence
over overlays with lower priority.  *Don't* use negative number."
  :type 'integer
  :group 'bm)


(defface bm-face '((t (:background "CornflowerBlue")))
  "Face used to highlight current line.")


(defcustom bm-wrap t
 "*Specify if bookmark search should wrap.

nil, don't wrap when there are no more bookmarks.
t, wrap.")


(defun bm-customize nil
  "Customize bm group"
  (interactive)
  (customize-group 'bm))


(defun bm-toggle-bookmark nil
  "Toggle bookmark in current buffer."
  (interactive)
  (let ((bookmark-start-list (mapcar 'overlay-start (bm-bookmarks))))
    ;; check if bookmark exists
    (if (member (line-beginning-position) bookmark-start-list)
	;; delete bookmark
	(delete-overlay (bm-bookmark-at (line-beginning-position)))

      ;; create new bookmark
      (let ((bm-overlay (make-overlay 
			 (line-beginning-position) 
			 (bm-end-position)))) ; to end of line
	(overlay-put bm-overlay 'face bm-face)
	(overlay-put bm-overlay 'priority bm-priority)
	(overlay-put bm-overlay 'modification-hooks '(bm-freeze-overlay))
	(overlay-put bm-overlay 'category "bm")))
      ))


(defun bm-end-position nil
  "Return the bookmark end position."
  (min (point-max) (+ 1 (line-end-position))))


(defun bm-freeze-overlay (overlay after begin end &optional len)
  "Prevent overlay from being extended to multiple lines."
  (if after
      (let ((bm-end (save-excursion
		      (goto-char (overlay-start overlay))
		      (bm-end-position))))
	(move-overlay overlay (overlay-start overlay) bm-end))))


(defun bm-lessp (first second)
  "Compare two bookmark overlays. Return t if first is less than second."
  (let ((first-start (overlay-start first))
	(second-start (overlay-start second)))
    (< first-start second-start)))


(defun bm-bookmarkp (bookmark)
  "Return t if bookmark is a bookmark."
  (if (and (overlayp bookmark) 
	   (string= (overlay-get bookmark 'category) "bm"))
      t
    nil))


(defun bm-bookmark-at (point)
  "Get bookmark at point."
  (let ((overlays (overlays-at point))
	(bookmark nil))
    (while overlays
      (if (bm-bookmarkp (car overlays))
	  (progn
	    (setq bookmark (car overlays))
	    (setq overlays nil))	; bookmark found
	(setq overlays (cdr overlays))	; keep looping
	))
    bookmark))


(defun bm-bookmarks nil
  "Return all bookmarks in buffer."
  ;; get all overlays
  (let ((overlays (append (car (overlay-lists)) (cdr (overlay-lists))))
	(overlay nil)
	(bookmarks nil))
    (while overlays
      (setq overlay (car overlays))
      ;; get all visible bookmarks
      (if (bm-bookmarkp overlay)
	   (if (bm-bookmark-at (overlay-start overlay))
	       ;; add visible bookmarks to list
	       (setq bookmarks (cons overlay bookmarks))
	     ;; delete invisible overlays
	     (delete-overlay overlay)))
      (setq overlays (cdr overlays)))
    
    (sort bookmarks 'bm-lessp)
    ))


(defun bm-goto-bookmark nil
  "Goto bookmark."
  (interactive)
  (let ((destination nil)
	(bookmark nil)
	(bookmarks (bm-bookmarks)))
    (if bookmarks
	(progn
	  (while bookmarks
	    (setq bookmark (car bookmarks))
	    (if (<= (overlay-start bookmark) (point))
		(setq bookmarks (cdr bookmarks)) ; keep looping
	      ;; bookmark found
	      (setq destination (overlay-start bookmark))
	      (setq bookmarks nil)))

	  ;; jump
	  (if destination
	      (goto-char destination)
	    (if (not bm-wrap)
		(message "No next bookmark.")
	      (goto-char (overlay-start (car (bm-bookmarks))))	      
	      (message "No next bookmark... wrapping."))
	    ))
      (message "No bookmarks defined."))
    ))


(defun bm-goto-bookmark-previous nil
  "Goto previous bookmark."
  (interactive)
  (let ((destination nil)
	(bookmark nil)
	(bookmarks (reverse (bm-bookmarks))))
    (if bookmarks
	(progn
	  (while bookmarks
	    (setq bookmark (car bookmarks))
	    (if (>= (overlay-start bookmark) (point))
		(setq bookmarks (cdr bookmarks)) ; keep looping
	      ;; bookmark found
	      (setq destination (overlay-start bookmark))
	      (setq bookmarks nil)))
    
	  ;; jump
	  (if destination
	      (goto-char destination)
	    (if (not bm-wrap)
		(message "No previous bookmark.")
	      (goto-char (overlay-start (car (reverse (bm-bookmarks)))))
	      (message "No previous bookmark... wrapping."))
	    ))
      (message "No bookmarks defined."))
    ))


(defun bm-delete-all nil
  "Delete all bookmarks in current buffer."
  (interactive)
  (mapcar 'delete-overlay (bm-bookmarks)))


(defun bm-toggle-wrapping nil
  "Toggle wrapping on/off, when searching for next bookmark."
  (interactive)
  (setq bm-wrap (not bm-wrap))
  (if bm-wrap
      (message "Wrapping on.")
    (message "Wrapping off.")))


;; bm.el ends here
(provide 'bm)
