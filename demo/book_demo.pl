:- module(book_demo, []).

/** <module> Book demo

Generates an HTML demo page for viewing books on the Web.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(wl/info/book/google_book)).

:- http_handler(root(book), book_demo, [id(book)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(book, 'books').



book_demo(_):-
	reply_html_page(wl_demo, title('Book demo'), \book_demo_body).

book_demo_body -->
	html([
	    style(
		'.stylename  {
		      font-family: monospace;
			     }
		#viewerdiv {
		   width: 800px;
		   height: 800px;
		 }'),
	    h1('Clocksin & Mellish'),
	    p('Presented with the Google Books Viewer'),
	    \google_book_viewer(book_opts)
	     ]).


book_opts(provider(google)).   % this is the default anyway
book_opts(id(viewerdiv)).      % id tag for the viewer
book_opts(book_id('ISBN:3540006788')).   % Clocksin & Mellish

