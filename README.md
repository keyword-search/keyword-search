<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [keyword-search.el](#keyword-searchel)
    - [Installation](#installation)
    - [Basic Configuration](#basic-configuration)
    - [Usage](#usage)
    - [keyword-search-extra-mode](#keyword-search-extra-mode)
    - [keyword-search-dessert-stomach-mode](#keyword-search-dessert-stomach-mode)
    - [Demo Video](#demo-video)

<!-- markdown-toc end -->


keyword-search.el
=================

Browser style keyword search for Emacs based on browse-apropos-url.

See <http://www.emacswiki.org/emacs/BrowseAproposURL>

Installation
------------

`keyword-search` is available on [MELPA](http://melpa.org).

You can install `keyword-search` with the following command.

* <kbd>M-x package-install [RET] keyword-search [RET]</kbd>

Basic Configuration
-------------------

You can set default search engine with the following command.

* <kbd>M-x customize-variable [RET] keyword-search-default [RET]</kbd>

You can append search engines with the following commands.

* <kbd>C-h v keyword-search-alist [RET]</kbd>
* <kbd>M-x customize-variable [RET] keyword-search-alist [RET]</kbd>

Please refer to the comment of `keyword-search.el`.

Usage
-----

1. <kbd>M-x keyword-search [RET]</kbd>
2. Choose search engine. <kbd>[TAB]</kbd> will autocomplete it.
3. Search query will be read from symbol at point, region or string in the minibuffer.

keyword-search-extra-mode
-------------------------

Mode for an alist of extra language services.
There are so many languages that it will be late at load time
if this mode is ON.
It takes 90 seconds to load it on an old computer.

Toggle ON/OFF:

* <kbd>M-x keyword-search-extra-mode [RET]</kbd>

keyword-search-dessert-stomach-mode
-----------------------------------

User-customizable mode.

```lisp
(require 'keyword-search-extra)

(defun keyword-search-my-fun ()
  "G7 of greenhouse gas emitters."
  (keyword-search-meta-alist
   (let (
	 (alist '(
		  ("greenhouse-gas-%s" . "http://%s.greenhouse-gas/%%s")
		  ))
	 (locale '(
		   "de"
		   "in"
		   "ja"
		   "kr"
		   "ru"
		   "us"
		   "zh-CN"
		   ))
	 (value)  ; make sure list starts empty
	 )
     (dolist (site-element alist value)
       (let (
	     (car-element (car site-element))
	     (cdr-element (cdr site-element))
	     )
	 (cond
	  ((string-equal car-element "greenhouse-gas-%s")
	   (dolist (l locale value)
	     (setq value (add-to-list 'value
				      (cons
				       (intern
					(format car-element l))
				       (format cdr-element l))
				      t))))))))))

(add-hook 'keyword-search-dessert-stomach-mode-hook 'keyword-search-my-fun nil)
(keyword-search-dessert-stomach-mode t)
```

If you want to toggle ON/OFF this mode, please append functions to
```keyword-search-dessert-stomach-mode-toggle-hook```:

```lisp
(add-hook 'keyword-search-dessert-stomach-mode-toggle-hook
	  'keyword-search-my-fun)
```

Demo Video
----------

* [Web Search from the Emacs](https://www.youtube.com/watch?v=IU4omPkG91M)
