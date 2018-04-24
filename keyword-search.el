;;; keyword-search.el --- browser keyword search from Emacs
;;
;; Authors: hugo and Richard Riley
;; Maintainer: Jens Petersen
;; Provisional maintainer: Akihiro Kuroiwa
;; Created: 29 Jun 2013
;; Keywords: web, search, keyword
;; X-URL: https://github.com/juhp/keyword-search
;; URL: https://github.com/keyword-search/keyword-search
;; Version: 0.2.1

;; Contributors:
;;  Hong Xu
;;  Steve Purcell
;;  Syohei YOSHIDA

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

;;; Code:

(require 'browse-url)

;;;###autoload
(defcustom keyword-search-alist
  '(
    (alc . "http://eow.alc.co.jp/search?q=%s")
    (cookpad-ja . "http://cookpad.com/search/%s")
    (cookpad-us . "https://cookpad.com/us/search/%s")
    (debpkg . "http://packages.debian.org/search?keywords=%s")
    (debpkg-contents . "https://packages.debian.org/file:%s")
    (dict-org . "http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=%s")
    (duckduckgo . "https://duckduckgo.com/?q=%s")
    (emacswiki . "https://duckduckgo.com/?q=%s+site%%3Aemacswiki.org&ia=web")
    (foldoc . "http://foldoc.org/%s")
    (github . "https://github.com/search?q=%s")
    (google . "http://www.google.com/search?q=%s")
    (google-books . "https://www.google.com/search?q=%s&tbm=bks")
    (google-finance . "http://www.google.com/finance?q=%s")
    (google-lucky . "http://www.google.com/search?btnI=I%%27m+Feeling+Lucky&q=%s")
    (google-images . "http://images.google.com/images?sa=N&tab=wi&q=%s")
    (google-groups . "http://groups.google.com/groups?q=%s")
    ;; (google-directory . "http://www.google.com/search?&sa=N&cat=gwd/Top&tab=gd&q=%s")
    (google-news . "http://news.google.com/news?sa=N&tab=dn&q=%s")
    (google-scholar . "https://scholar.google.com/scholar?q=%s")
    (google-translate . "http://translate.google.com/?source=osdd#auto|auto|%s")
    (google-translate-en-ja . "http://translate.google.com/?source=osdd#en|ja|%s")
    (google-translate-ja-en . "http://translate.google.com/?source=osdd#ja|en|%s")
    (hackage . "http://hackage.haskell.org/package/%s")
    (hayoo . "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query=%s")
    (hdiff . "http://hdiff.luite.com/cgit/%s")
    (jisho-org . "http://jisho.org/search/%s")
    (koji . "http://koji.fedoraproject.org/koji/search?match=glob&type=package&terms=%s")
    (melpa . "http://melpa.org/#/%s")
    (pypi . "https://pypi.python.org/pypi?%%3Aaction=search&term=%s&submit=search")
    (readthedocs-org . "https://readthedocs.org/search/?q=%s")
    (slashdot . "http://www.osdn.com/osdnsearch.pl?site=Slashdot&query=%s")
    (startpage . "https://startpage.com/do/search?cat=web&query=%s")
    (ubupkg . "http://packages.ubuntu.com/search?keywords=%s")
    (weblio-en-ja . "http://ejje.weblio.jp/content/%s")
    (wikipedia . "http://en.wikipedia.org/wiki/%s")
    (wikipedia-ja . "http://ja.wikipedia.org/wiki/%s")
    (yahoo . "http://search.yahoo.com/search?p=%s")
    (youtube . "http://www.youtube.com/results?search_query=%s")
    )
  "An alist of pairs (KEYWORD . URL) where KEYWORD is a keyword symbol \
and URL string including '%s' is the search url.

\"%\" should be replaced with \"%%\"."
  :type '(alist
	  :key-type (symbol :tag "Name")
	  :value-type (string :tag "URL"))
  :group 'keyword-search
  )

;;;###autoload
(defcustom keyword-search-default 'google
  "Default search engine used by `keyword-search' and `keyword-search-quick' \
if none given."
  :type 'symbol
  :group 'keyword-search
  )

(defun keyword-search-get-query ()
  "Return the selected region (if any) or the symbol at point.
This function is based on `engine-mode.el'."
  (if (use-region-p)
      (let (
	    (r-list
	     '(
	       ("[\u3000 \t\n]+" . " ")
	       ("^\s-*\\|\s-*$" . "")
	       ("\\(\\s(\\) \\(\\s(\\)" . "\\1\\2")
	       ("\\(\\s)\\) \\(\\s)\\)" . "\\1\\2")
	       ("\\(\\s(\\) \\(\\s(\\)" . "\\1\\2")
	       ("\\(\\s)\\) \\(\\s)\\)" . "\\1\\2")
	       ))
	    (value (buffer-substring (region-beginning) (region-end)))
	    )
	(dolist (element r-list value)
	  (setq value (replace-regexp-in-string (car element) (cdr element) value))))
    (thing-at-point 'symbol)))

;;;###autoload
(defun keyword-search (key query &optional new-window)
  "Read a keyword KEY from `keyword-search-alist' with completion \
and then read a search term QUERY defaulting to the symbol at point.
It then does a websearch of the url associated to KEY using `browse-url'.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new window, if possible, otherwise use
a random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

When called non-interactively, optional third argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive
   (let ((key
	  (completing-read
	   (format "Keyword search (default %s): " keyword-search-default)
	   keyword-search-alist nil t nil nil (symbol-name keyword-search-default))))
     (list key
	   (let ((thing (keyword-search-get-query)))
	     (read-string
	      (if thing
		  (format (concat key " (%s): " ) thing)
		(concat key ": " ))
	      nil nil thing)))))
  (let ((url (cdr (assoc (intern-soft key) keyword-search-alist))))
    (browse-url (format url (url-hexify-string query)) new-window)))

;;;###autoload
(defun keyword-search-at-point (key &optional new-window)
  "Read a keyword KEY from `keyword-search-alist' with completion \
and does a websearch of the symbol at point using `browse-url'.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new window, if possible, otherwise use
a random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive
   (list (completing-read
	  (format "Keyword search at point (default %s): " keyword-search-default)
	  keyword-search-alist nil t nil nil (symbol-name keyword-search-default))))
  (let ((thing (keyword-search-get-query))
	(url (cdr (assoc (intern-soft key) keyword-search-alist))))
    (browse-url (format url (url-hexify-string thing)) new-window)))

;;;###autoload
(defun keyword-search-quick (text)
  "A wrapper of `keyword-search' which read the keyword and \
search query in a single input as argument TEXT from the minibuffer."
  (interactive
   (list (read-string "Keyword search quick: ")))
  (let* ((words (split-string-and-unquote text))
	 (key (car words))
	 (keywordp (assoc (intern-soft key) keyword-search-alist))
	 (keyword (if keywordp key
		    keyword-search-default)))
    (keyword-search (intern-soft keyword)
		    (combine-and-quote-strings (if keywordp (cdr words) words)))))

;;;###autoload
(defun keyword-search-quick-additionally (text)
  "In addition to a query of `keyword-search-get-query', \
read a string TEXT from the minibuffer.

This is a wrapper of `keyword-search-quick'"
  (interactive
   (list (let ((thing (keyword-search-get-query)))
	   (read-string "Keyword search quick additionally: " thing))))
  (keyword-search-quick text))

(provide 'keyword-search)
;;; keyword-search.el ends here
