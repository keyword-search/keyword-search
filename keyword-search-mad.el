;;; keyword-search-mad.el --- additional websites for experts or manias
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

;; There is one pair of hook and mode for additional websites
;; where experts or manias only use.

;;; Code:

(require 'keyword-search)
(require 'keyword-search-extra)

(defvar keyword-search-mad-mode-hook nil
  "Hook for `keyword-search-mad-mode'.")

;;;###autoload
(define-minor-mode keyword-search-mad-mode
  "Mode for additional websites where experts or manias only use.

Don't turn on this mode unless you choose to be mad."
  :lighter " k-mad"
  :global t
  :group 'keyword-search
  (if keyword-search-mad-mode
	(add-hook 'keyword-search-mad-mode-hook 'keyword-search-expert-alist nil t)
    (remove-hook 'keyword-search-mad-mode-hook 'keyword-search-expert-alist t)
    (custom-reevaluate-setting 'keyword-search-alist)))

(defun keyword-search-expert-alist ()
  "Using dolist, append additional association list for experts."
  (keyword-search-meta-alist
   (let (
	 (alist '(
		  ("cookpad-%s" . "https://cookpad.com/%s/%%s?ref=search.suggestion")
		  ("stackexchange-%s" . "http://%s.stackexchange.com/search?q=%%s")
		  ("stackoverflow-%s" . "http://%s.stackoverflow.com/search?q=%%s")
		  ("wikibooks-%s" . "https://%s.wikibooks.org/wiki/%%s")
		  ("wikipedia-%s" . "https://%s.wikipedia.org/wiki/%%s")
		  ("wikiquote-%s" . "https://%s.wikiquote.org/wiki/%%s")
		  ("wikisource-%s" . "https://%s.wikisource.org/wiki/%%s")
		  ("wiktionary-%s" . "https://%s.wiktionary.org/wiki/%%s")
		  ))
	 (cookpad-locale '(
			   "ar/buscar"
			   "arabic/search"
			   "bo/buscar"
			   "cl/buscar"
			   "co/buscar"
			   "cr/buscar"
			   "cu/buscar"
			   "do/buscar"
			   "ec/buscar"
			   "eeuu/buscar"
			   "es/buscar"
			   "gt/buscar"
			   "hn/buscar"
			   "id/cari"
			   "mx/buscar"
			   "ni/buscar"
			   "pa/buscar"
			   "pe/buscar"
			   "ph/i-search"
			   "pri/buscar"
			   "py/buscar"
			   "sv/buscar"
			   "th/search"
			   "uk/search"
			   "us/search"
			   "uy/buscar"
			   "ve/buscar"
			   "vn/tim-kiem"
			   ))
	 (stackexchange-service '(
				  "3dprinting"
				  "academia"
				  "alcohol"
				  "android"
				  "anime"
				  "apple"
				  "arduino"
				  "area51"
				  "astronomy"
				  "aviation"
				  "bicycles"
				  "biology"
				  "bitcoin"
				  "blender"
				  "boardgames"
				  "bricks"
				  "buddhism"
				  "chemistry"
				  "chess"
				  "chinese"
				  "christianity"
				  "civicrm"
				  "codegolf"
				  "codereview"
				  "coffee"
				  "cogsci"
				  "communitybuilding"
				  "computergraphics"
				  "cooking"
				  "craftcms"
				  "crafts"
				  "crypto"
				  "cs"
				  "cstheory"
				  "datascience"
				  "dba"
				  "diy"
				  "drupal"
				  "dsp"
				  "earthscience"
				  "ebooks"
				  "economics"
				  "electronics"
				  "elementaryos"
				  "ell"
				  "emacs"
				  "engineering"
				  "english"
				  "ethereum"
				  "expatriates"
				  "expressionengine"
				  "fitness"
				  "freelancing"
				  "french"
				  "gamedev"
				  "gaming"
				  "gardening"
				  "genealogy"
				  "german"
				  "gis"
				  "graphicdesign"
				  "ham"
				  "hardwarerecs"
				  "health"
				  "hermeneutics"
				  "hinduism"
				  "history"
				  "homebrew"
				  "hsm"
				  "islam"
				  "italian"
				  "japanese"
				  "joomla"
				  "judaism"
				  "languagelearning"
				  "latin"
				  "law"
				  "lifehacks"
				  "linguistics"
				  "magento"
				  "martialarts"
				  "math"
				  "matheducators"
				  "mathematica"
				  "mechanics"
				  "meta"
				  "money"
				  "movies"
				  "music"
				  "musicfans"
				  "mythology"
				  "networkengineering"
				  "opendata"
				  "opensource"
				  "outdoors"
				  "parenting"
				  "patents"
				  "pets"
				  "philosophy"
				  "photo"
				  "physics"
				  "pm"
				  "poker"
				  "politics"
				  "portuguese"
				  "productivity"
				  "programmers"
				  "puzzling"
				  "quant"
				  "raspberrypi"
				  "retrocomputing"
				  "reverseengineering"
				  "robotics"
				  "rpg"
				  "rus"
				  "russian"
				  "salesforce"
				  "scicomp"
				  "scifi"
				  "security"
				  "sharepoint"
				  "skeptics"
				  "softwarerecs"
				  "sound"
				  "space"
				  "spanish"
				  "sports"
				  "sqa"
				  "startups"
				  "stats"
				  "sustainability"
				  "tex"
				  "tor"
				  "travel"
				  "tridion"
				  "unix"
				  "ux"
				  "vi"
				  "video"
				  "webapps"
				  "webmasters"
				  "windowsphone"
				  "woodworking"
				  "wordpress"
				  "workplace"
				  "worldbuilding"
				  "writers"
				  ))
	 (stackoverflow-locale '(
				 "es"
				 "ja"
				 "pt"
				 "ru"
				 ))
	 (wikimedia-locale '(
			     "aa"
			     "ab"
			     "ace"
			     "ady"
			     "af"
			     "ak"
			     "als"
			     "am"
			     "an"
			     "ang"
			     "ar"
			     "arc"
			     "arz"
			     "as"
			     "ast"
			     "av"
			     "ay"
			     "az"
			     "azb"
			     "ba"
			     "bar"
			     "bat-smg"
			     "bcl"
			     "be"
			     "be-x-old‎"
			     "bg"
			     "bh"
			     "bi"
			     "bjn"
			     "bm"
			     "bn"
			     "bo"
			     "bpy"
			     "br"
			     "bs"
			     "bug"
			     "bxr"
			     "ca"
			     "cbk-zam"
			     "cdo"
			     "ce"
			     "ceb"
			     "ch"
			     "cho"
			     "chr"
			     "chy"
			     "ckb"
			     "co"
			     "cr"
			     "crh"
			     "cs"
			     "csb"
			     "cu"
			     "cv"
			     "cy"
			     "da"
			     "de"
			     "diq"
			     "dsb"
			     "dv"
			     "dz"
			     "ee"
			     "el"
			     "eml"
			     "en"
			     "eo"
			     "es"
			     "et"
			     "eu"
			     "ext"
			     "fa"
			     "ff"
			     "fi"
			     "fiu-vro"
			     "fj"
			     "fo"
			     "fr"
			     "frp"
			     "frr"
			     "fur"
			     "fy"
			     "ga"
			     "gag"
			     "gan"
			     "gd"
			     "gl"
			     "glk"
			     "gn"
			     "gom"
			     "got"
			     "gu"
			     "gv"
			     "ha"
			     "hak"
			     "haw"
			     "he"
			     "hi"
			     "hif"
			     "ho"
			     "hr"
			     "hsb"
			     "ht"
			     "hu"
			     "hy"
			     "hz"
			     "ia"
			     "id"
			     "ie"
			     "ig"
			     "ii"
			     "ik"
			     "ilo"
			     "io"
			     "is"
			     "it"
			     "iu"
			     "ja"
			     "jbo"
			     "jv"
			     "ka"
			     "kaa"
			     "kab"
			     "kbd"
			     "kg"
			     "ki"
			     "kj"
			     "kk"
			     "kl"
			     "km"
			     "kn"
			     "ko"
			     "koi"
			     "kr"
			     "krc"
			     "ks"
			     "ksh"
			     "ku"
			     "kv"
			     "kw"
			     "ky"
			     "la"
			     "lad"
			     "lb"
			     "lbe"
			     "lez"
			     "lg"
			     "li"
			     "lij"
			     "lmo"
			     "ln"
			     "lo"
			     "lrc"
			     "lt"
			     "ltg"
			     "lv"
			     "mai"
			     "map-bms"
			     "mdf"
			     "mg"
			     "mh"
			     "mhr"
			     "mi"
			     "min"
			     "mk"
			     "ml"
			     "mn"
			     "mo"
			     "mr"
			     "mrj"
			     "ms"
			     "mt"
			     "mus"
			     "mwl"
			     "my"
			     "myv"
			     "mzn"
			     "na"
			     "nah"
			     "nap"
			     "nds"
			     "nds-nl"
			     "ne"
			     "new"
			     "ng"
			     "nl"
			     "nn"
			     "no"
			     "nov"
			     "nrm"
			     "nso"
			     "nv"
			     "ny"
			     "oc"
			     "om"
			     "or"
			     "os"
			     "pa"
			     "pag"
			     "pam"
			     "pap"
			     "pcd"
			     "pdc"
			     "pfl"
			     "pi"
			     "pih"
			     "pl"
			     "pms"
			     "pnb"
			     "pnt"
			     "ps"
			     "pt"
			     "qu"
			     "rm"
			     "rmy"
			     "rn"
			     "ro"
			     "roa-rup"
			     "roa-tara"
			     "ru"
			     "rue"
			     "rw"
			     "sa"
			     "sah"
			     "sc"
			     "scn"
			     "sco"
			     "sd"
			     "se"
			     "sg"
			     "sh"
			     "si"
			     "simple"
			     "sk"
			     "sl"
			     "sm"
			     "sn"
			     "so"
			     "sq"
			     "sr"
			     "srn"
			     "ss"
			     "st"
			     "stq"
			     "su"
			     "sv"
			     "sw"
			     "szl"
			     "ta"
			     "te"
			     "tet"
			     "tg"
			     "th"
			     "ti"
			     "tk"
			     "tl"
			     "tn"
			     "to"
			     "tpi"
			     "tr"
			     "ts"
			     "tt"
			     "tum"
			     "tw"
			     "ty"
			     "tyv"
			     "udm"
			     "ug"
			     "uk"
			     "ur"
			     "uz"
			     "ve"
			     "vec"
			     "vep"
			     "vi"
			     "vls"
			     "vo"
			     "wa"
			     "war"
			     "wo"
			     "wuu"
			     "xal"
			     "xh"
			     "xmf"
			     "yi"
			     "yo"
			     "za"
			     "zea"
			     "zh"
			     "zh-classical"
			     "zh-min-nan"
			     "zh-yue"
			     "zu"
			     ))
	 (value '(
		  (arxiv-org . "http://arxiv.org/find/all/1/all:+%s/0/1/0/all/0/1")
		  (arxiv-org-and . "http://arxiv.org/find/all/1/all:+AND+%s/0/1/0/all/0/1")
		  (askubuntu . "http://askubuntu.com/search?q=%s")
		  (cinii-en . "http://ci.nii.ac.jp/search?lang=en&q=%s&range=0&sortorder=1&start=1&count=20")
		  (cinii-ja . "http://ci.nii.ac.jp/search?lang=ja&q=%s&range=0&sortorder=1&start=1&count=20")
		  (grabcad . "https://grabcad.com/library?per_page=20&query=%s")
		  (instagram-account . "https://www.instagram.com/$s")
		  (instagram-tags . "https://www.instagram.com/explore/tags/%s")
		  (j-stage-en . "https://www.jstage.jst.go.jp/result?item1=4&word1=%s")
		  (j-stage-ja . "https://www.jstage.jst.go.jp/result/-char/ja/?item1=4&word1=%s")
		  (mathoverflow . "http://mathoverflow.net/search?q=%s")
		  (nature . "http://www.nature.com/search?journal=nature%%2Cnews&q=%s")
		  (nhkworld . "http://www2.nhk.or.jp/nhkworld/en/search/?qt=%s&charset=utf-8&lk=1&la=en&qi=3&col=nhkworld")
		  (pmc . "http://www.ncbi.nlm.nih.gov/pmc/?term=%s")
		  (pubmed . "http://www.ncbi.nlm.nih.gov/pubmed/?term=%s")
		  (rfc-number . "https://www.rfc-editor.org/search/rfc_search_detail.php?rfc=%s&pubstatus[]=Any&pub_date_type=any")
		  (rfc-title-keyword . "https://www.rfc-editor.org/search/rfc_search_detail.php?title=%s&pubstatus[]=Any&pub_date_type=any")
		  (santa-fe-institute . "http://www.santafe.edu/search/results/?query=%s")
		  (sciencemag . "http://www.sciencemag.org/search/%s")
		  (serverfault . "http://serverfault.com/search?q=%s")
		  (stackapps . "http://stackapps.com/search?q=%s")
		  (stackoverflow . "http://stackoverflow.com/search?q=%s")
		  (starwars-wikia . "http://starwars.wikia.com/wiki/Special:Search?search=%s&go=&fulltext=Search")
		  (superuser . "http://superuser.com/search?q=%s")
		  (tver . "http://tver.jp/search/?keyword=%s")
		  (twitter-hashtag . "https://twitter.com/hashtag/%s")
		  (twitter-search . "https://twitter.com/search?q=%s&src=typd")
		  (weblio . "http://www.weblio.jp/content/%s")
		  (whotwi-ar . "http://ar.whotwi.com/%s")
		  (whotwi-en . "http://en.whotwi.com/%s")
		  (whotwi-ja . "http://ja.whotwi.com/%s")
		  (world-heritage-centre . "http://whc.unesco.org/en/search/?criteria=%s")
		  ))
	 )
     (dolist (site-element alist value)
       (let (
	     (car-element (car site-element))
	     (cdr-element (cdr site-element))
	     )
	 (cond
	  ((string-equal car-element "cookpad-%s")
	   (dolist (l cookpad-locale value)
	     (setq value (add-to-list 'value
				      (cons
				       (intern
					(format car-element l))
				       (format cdr-element l))
				      t))))
	  ((string-equal car-element "stackexchange-%s")
	   (dolist (s stackexchange-service value)
	     (setq value (add-to-list 'value
				      (cons
				       (intern
					(format car-element s))
				       (format cdr-element s))
				      t))))
	  ((string-equal car-element "stackoverflow-%s")
	   (dolist (l stackoverflow-locale value)
	     (setq value (add-to-list 'value
	  			      (cons
	  			       (intern
	  				(format car-element l))
	  			       (format cdr-element l))
	  			      t))))
	  (t
	   (dolist (l wikimedia-locale value)
	     (setq value (add-to-list 'value
				      (cons
				       (intern
					(format car-element l))
				       (format cdr-element l))
				      t))))))))))

(provide 'keyword-search-mad)
;;; keyword-search-mad.el ends here
