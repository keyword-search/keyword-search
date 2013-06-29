;;; keyword-search.el --- browser keyword search from Emacs
;;
;; Authors: hugo and Richard Riley
;; Maintainer: Jens Petersen
;; Created: 29 Jun 2013
;; Keywords: web, search, keyword
;; X-URL: https://github.com/juhp/keyword-search
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This code is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is based on the code snippets at
;; http://www.emacswiki.org/emacs/BrowseAproposURL
;; (maybe if a complete file had been posted there
;; I would not have forked this off.

;; To use:

;; (load "keyword-search")
;; (define-key mode-specific-map [?B] 'keyword-search)

;; Example of a direct search binding:
;;
;; (eval-after-load 'haskell-mode
;;   '(define-key haskell-mode-map (kbd "C-c h")
;;      (lambda ()
;;        (interactive)
;;        (keyword-search "hayoo"))))

(require 'browse-url)

(defvar keyword-search-alist
  '(("google" .
     "http://www.google.com/search?q=%s")
    ("google-lucky" .
     "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=%s")
    ("google-linux" .
     "http://www.google.com/linux?q=%s")
    ("google-images" .
     "http://images.google.com/images?sa=N&tab=wi&q=%s")
    ("google-groups" .
     "http://groups.google.com/groups?q=%s")
    ("google-directory" .
     "http://www.google.com/search?&sa=N&cat=gwd/Top&tab=gd&q=%s")
    ("github" .
     "https://github.com/search?q=%s")
    ("google-news" .
     "http://news.google.com/news?sa=N&tab=dn&q=%s")
    ("slashdot" .
     "http://www.osdn.com/osdnsearch.pl?site=Slashdot&query=%s")        
    ("emacswiki" .
     "http://www.emacswiki.org/cgi-bin/wiki?search=%s")
    ("arda" .
     "http://www.glyphweb.com/arda/")
    ("hayoo" .
     "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query=%s")
    ))

(defvar keyword-search-default "google")

(defun keyword-search (key &optional new-window)
  (interactive
     (list (completing-read
	    (format "Keyword search (%s): " keyword-search-default)
	    apropos-url-alist nil t nil nil keyword-search-default)))
  (let ((thing (thing-at-point 'symbol)))
    (setq thing
	  (read-string
	   (if thing
	       (format (concat key " (%s): " ) thing)
	     (concat key ": " ))
	   nil nil thing))
    (let ((url (cdr (assoc key apropos-url-alist))))
      (browse-url (format url thing) new-window))))
