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
