;;; keyword-search.el --- browser keyword search from Emacs
;;
;; Authors: hugo and Richard Riley
;; Maintainer: Jens Petersen
;; Created: 29 Jun 2013
;; Keywords: web, search, keyword
;; X-URL: https://github.com/juhp/keyword-search
;; Version: 0.2

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
;; I would not have forked this off).

;; It provides 3 functions `keyword-search', `keyword-search-at-point'
;; and `keyword-search-quick'.
;;
;; `keyword-search': provides completion on keywords and then reads
;; a search term defaulting to the symbol at point.
;;
;; `keyword-search-at-point': reads a keyword with completion and then
;; searchs with the symbol at point.
;;
;; `keyword-search-quick': reads a query in one line if it does not
;; start with a keyword it uses `keyword-search-default'.

;; To use:

;; (load "keyword-search")
;; (define-key mode-specific-map [?b] 'keyword-search)
;; (define-key mode-specific-map [?B] 'keyword-search-quick)

;; Example of a direct search binding:
;;
;; (eval-after-load 'haskell-mode
;;   '(define-key haskell-mode-map (kbd "C-c h")
;;      (lambda ()
;;        (interactive)
;;        (keyword-search "hayoo"))))

(require 'browse-url)

(defvar keyword-search-alist
  '(("debpkg" . "http://packages.debian.org/search?keywords=%s")
    ("emacswiki" . "http://www.emacswiki.org/cgi-bin/wiki?search=%s")
    ("github" . "https://github.com/search?q=%s")
    ("google" . "http://www.google.com/search?q=%s")
    ("google-books" . "https://www.google.com/search?q=%s&tbm=bks")
    ("google-lucky" . "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=%s")
    ("google-linux" . "http://www.google.com/linux?q=%s")
    ("google-images" . "http://images.google.com/images?sa=N&tab=wi&q=%s")
    ("google-groups" . "http://groups.google.com/groups?q=%s")
    ("google-directory" . "http://www.google.com/search?&sa=N&cat=gwd/Top&tab=gd&q=%s")
    ("google-news" . "http://news.google.com/news?sa=N&tab=dn&q=%s")
    ("hackage" . "http://hackage.haskell.org/package/%s")
    ("hayoo" . "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query=%s")
    ("hdiff" . "http://hdiff.luite.com/cgit/%s")
    ("koji" . "http://koji.fedoraproject.org/koji/search?match=glob&type=package&terms=%s")
    ("slashdot" . "http://www.osdn.com/osdnsearch.pl?site=Slashdot&query=%s")
    ("ubupkg" . "http://packages.ubuntu.com/search?keywords=%s")
    ("wikipedia" . "http://en.wikipedia.org/wiki/%s")
    ("yahoo" . "http://search.yahoo.com/search?p=%s")
    ("youtube" . "http://www.youtube.com/results?search_query=%s")
    )
  "A alist of pairs (KEYWORD . URL) where KEYWORD is a keyword string
and URL including '%s' is the search url."
  )

(defvar keyword-search-default
  "Default keyword used by `keyword-search' and `keyword-search-quick'
if none given."
  "google")

(defun keyword-search (key query &optional new-window)
  "Reads a keyword KEY from `keyword-search-alist' with completion
and then reads a search term QUERY defaulting to the symbol at point.
It then does a websearch of the url associated to KEY using `browse-url'."
  (interactive
     (list (completing-read
	    (format "Keyword search (%s): " keyword-search-default)
	    keyword-search-alist nil t nil nil keyword-search-default)
	   (let ((thing (thing-at-point 'symbol)))
	     (read-string
	      (if thing
		  (format (concat key " (%s): " ) thing)
		(concat key ": " ))
	      nil nil thing))))
  (let ((url (cdr (assoc key keyword-search-alist))))
    (browse-url (format url query) new-window)))

(defun keyword-search-at-point (key &optional new-window)
  "Reads a keyword KEY from `keyword-search-alist' with completion
and does does a websearch of the symbol at point using `browse-url'."
  (interactive
     (list (completing-read
	    (format "Keyword search (%s): " keyword-search-default)
	    keyword-search-alist nil t nil nil keyword-search-default)))
  (let ((thing (thing-at-point 'symbol))
	(url (cdr (assoc key keyword-search-alist))))
    (browse-url (format url thing) new-window)))

(defun keyword-search-quick (text)
  "A wrapper of `keyword-search' which reads the keyword and
search query in a single input."
  (interactive
   (list (read-string "Keyword search quick: ")))
  (let* ((words (split-string-and-unquote text))
	 (key (car words))
	 (keywordp (assoc key keyword-search-alist))
	 (keyword (if keywordp key
		    keyword-search-default)))
    (keyword-search keyword (combine-and-quote-strings (if keywordp (cdr words) words)))))

(provide 'keyword-search)
