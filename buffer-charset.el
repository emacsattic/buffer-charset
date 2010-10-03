;;; buffer-charsets.el --- show usage of charsets in a buffer

;; Copyright (C) 2002 Joanna Pluta<joanna_p@poczta.gazeta.pl>.

;; Keywords: mule, hi-lock

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to <thomas.link@a1.net>) or
;; from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
;; MA 02139, USA.

;;; Commentary:

;;   - display-buffer-charsets
;;     Displays list of charsets used in current buffer.

;;   - show-buffer-charset-characters C-x w c

;;     Uses hi-lock-mode to highlight in current buffer characters
;;     from chosen charset.
;;     Creates and adds to local history (hi-lock-interactive-patterns)
;;     regexpx matching characters from a given charset.
;;     Characters from different charsets can be highlighted by
;;     different colors according to chosen face.
 
;;   - unhighlight-charset  C-x w u 
;;     unhighlights characters from given charset
;;  
 
 

;;; Code:

;;; 

(require 'hi-lock)

(defun charset-chars-regexp (charset)
 (let ((dim (charset-dimension charset))
	(chars (charset-chars charset))
	(plane (charset-iso-graphic-plane charset))
	min
	max)
      (cond ((eq charset 'eight-bit-control)
	   (setq min 128 max 159))
	  ((eq charset 'eight-bit-graphic)
	   (setq min 160 max 255))
	  (t
	   (if (= chars 94)
	       (setq min 33 max 126)
	     (setq min 32 max 127))
	   (or (= plane 0)
	       (setq min (+ min 128) max (+ max 128)))))
    (if (= dim 1)
	(format "[%c-%c]" (make-char charset min) (make-char charset max))
     (format "[%c-%c]" 
	     (make-char charset min min) (make-char charset max max)))))

(defun buffer-charsets ()
 (find-charset-region (point-min) (point-max)))

(defun display-buffer-charsets ()
  "Displays list of charsets used in current buffer"
  (interactive)
  (let ((charsets (buffer-charsets))
	(curr-buf-name (current-buffer)))
    (with-output-to-temp-buffer "*Buffer Charsets*"
      (save-excursion
	(set-buffer standard-output)
	(insert 
	 (format "Buffer %s uses the following charsets:\n" curr-buf-name))
	(while charsets
	  (insert (symbol-name (car charsets)))
	  (insert "\n")
	  (setq charsets (cdr charsets)))))))

(defun charset-alist (charset-list)
 (let ((l (charset-list))
       charset-alist)
  (while l 
    (setq charset-alist (cons (list (symbol-name (car l))) charset-alist))		 
    (setq l (cdr l)))
  charset-alist))


(defun show-buffer-charset-characters (charset face)
 "Uses hi-lock-mode to highlight by face characters of charset."
  (interactive 
   (let ((completion-ignore-case t))
   (list 
    (completing-read "Charset:" 
		     (charset-alist (buffer-charsets))  nil t nil nil)
    (hi-lock-read-face-name))))
    (highlight-regexp (charset-chars-regexp (intern charset)) face))
    
(defun unhighlight-charset (charset)
 (interactive
  (let ((completion-ignore-case t))
   (list 
    (completing-read "Charset:" 
		     (charset-alist (buffer-charsets))  nil t nil nil))))
    (unhighlight-regexp (charset-chars-regexp  (intern charset))))

(define-key hi-lock-map "\C-xwc" 'show-buffer-charset-characters)
(define-key hi-lock-map "\C-xwu" 'unhighlight-charset)

;;; mule-utils.el ends here



