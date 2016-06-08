;;; keyword-search-extra.el --- additional language services for keyword-search
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

;; There are two pairs of hooks and modes for additional language services.

;; `keyword-search-extra-mode': concatenates extra language services when it is ON.
;; It takes a long time to load it.

;; `keyword-search-dessert-stomach-mode': executes your functions added
;; in `keyword-search-dessert-stomach-mode-hook' or
;; `keyword-search-dessert-stomach-mode-toggle-hook'.

;; To use:

;; (add-hook 'keyword-search-dessert-stomach-mode-hook
;; 	  (lambda ()
;; 	    (add-to-list 'keyword-search-alist '(my-url . "http://my-url/%s"))))

;;; Code:

(require 'cl-lib)
(require 'keyword-search)

(defvar keyword-search-extra-mode-hook nil
  "Hook for `keyword-search-extra-mode'.")

(defvar keyword-search-dessert-stomach-mode-hook nil
  "Hook for `keyword-search-dessert-stomach-mode'.")

(defvar keyword-search-dessert-stomach-mode-toggle-hook nil
  "Hook for toggling `keyword-search-dessert-stomach-mode'.")

;;;###autoload
(define-minor-mode keyword-search-extra-mode
  "Mode for an alist of extra language services.
There are so many languages that it will be late at load time \
if this mode is ON.
It takes 90 seconds to load it on an old computer.

  If you believe \"Simple is best\", deactivate this mode."
  :lighter " k-extra"
  :global t
  :group 'keyword-search
  (if keyword-search-extra-mode
      (add-hook 'keyword-search-extra-mode-hook 'keyword-search-locale-alist nil t)
    (remove-hook 'keyword-search-extra-mode-hook 'keyword-search-locale-alist t)
    (custom-reevaluate-setting 'keyword-search-alist)))

;;;###autoload
(define-minor-mode keyword-search-dessert-stomach-mode
  "User-customizable mode.
You can append functions to `keyword-search-dessert-stomach-mode-hook' by `add-hook'.

If you want to turn ON/OFF this mode, please append functions to \
`keyword-search-dessert-stomach-mode-toggle-hook'."
  :lighter " k-stomach"
  :global t
  :group 'keyword-search
  (if keyword-search-dessert-stomach-mode
      (run-hooks 'keyword-search-dessert-stomach-mode-toggle-hook)
    (custom-reevaluate-setting 'keyword-search-alist)))

(defun keyword-search-locale-alist ()
  "Using dolist, append locale association list."
  (keyword-search-meta-alist
   (let (
	 (alist '(
		  ("google-translate-%s-%s" . "http://translate.google.com/?source=osdd#%s|%s|%%s")
		  ("weblio-%s" . "http://%s.weblio.jp/content/%%s")
		  ))
	 (google-locale '(
			  "af"
			  "am"
			  "ar"
			  "az"
			  "be"
			  "bg"
			  "bn"
			  "bs"
			  "ca"
			  "ceb"
			  "co"
			  "cs"
			  "cy"
			  "da"
			  "de"
			  "el"
			  "en"
			  "eo"
			  "es"
			  "et"
			  "eu"
			  "fa"
			  "fi"
			  "fr"
			  "fy"
			  "ga"
			  "gd"
			  "gl"
			  "gu"
			  "ha"
			  "haw"
			  "hi"
			  "hmn"
			  "hr"
			  "ht"
			  "hu"
			  "hy"
			  "id"
			  "ig"
			  "is"
			  "it"
			  "iw"
			  "ja"
			  "jw"
			  "ka"
			  "kk"
			  "km"
			  "kn"
			  "ko"
			  "ku"
			  "ky"
			  "la"
			  "lb"
			  "lo"
			  "lt"
			  "lv"
			  "mg"
			  "mi"
			  "mk"
			  "ml"
			  "mn"
			  "mr"
			  "ms"
			  "mt"
			  "my"
			  "ne"
			  "nl"
			  "no"
			  "ny"
			  "pa"
			  "pl"
			  "ps"
			  "pt"
			  "ro"
			  "ru"
			  "sd"
			  "si"
			  "sk"
			  "sl"
			  "sm"
			  "sn"
			  "so"
			  "sq"
			  "sr"
			  "st"
			  "su"
			  "sv"
			  "sw"
			  "ta"
			  "te"
			  "tg"
			  "th"
			  "tl"
			  "tr"
			  "uk"
			  "ur"
			  "uz"
			  "vi"
			  "xh"
			  "yi"
			  "yo"
			  "zh-CN"
			  "zh-TW"
			  "zu"
			  ))
	 (weblio-service '(
			   "cjjc"
			   "kjjk"
			   "kobun"
			   "njjn"
			   "shuwa"
			   "thesaurus"
			   "tjjt"
			   "vjjv"
			   ))
	 (value)  ; make sure list starts empty
	 )
     (dolist (site-element alist value)
       (let (
	     (car-element (car site-element))
	     (cdr-element (cdr site-element))
	     )
	 (cond
	  ((string-equal car-element "google-translate-%s-%s")
	   (dotimes (l (length google-locale) value)
	     (dotimes (m (length google-locale) value)
	       (setq value (add-to-list 'value
					(unless (equal (elt google-locale l) (elt google-locale m))
					  (cons
					   (intern
					    (format car-element (elt google-locale l)
						    (elt google-locale m)))
					   (format cdr-element (elt google-locale l)
						   (elt google-locale m))))
					t)))))
	  ((string-equal car-element "weblio-%s")
	   (dolist (s weblio-service value)
	     (setq value (add-to-list 'value
				      (cons
				       (intern
					(format car-element s))
				       (format cdr-element s))
				      t))))))))))

(defun keyword-search-meta-alist (additional-alist)
  "Concatenate `keyword-search-alist' with ADDITIONAL-ALIST and remove duplicates."
  (setq keyword-search-alist
	(cl-remove-if-not 'identity
			  (cl-remove-duplicates
			   (cl-sort
			    (cl-concatenate 'list keyword-search-alist additional-alist)
			    #'string< :key #'car)
			   :test #'equal :from-end t :key #'car))))

(provide 'keyword-search-extra)
;;; keyword-search-extra.el ends here
