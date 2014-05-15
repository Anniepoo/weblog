:- module(books_demo, []).
/** <module>  Demo handler for generic books display

*/
% basic dispatch
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(weblog(info/books/google/books)).

:- http_handler(root(books), books_demo_page, [id(books)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(books, 'Books').

books_demo_page(_Request) :-
	reply_html_page(
	    title('Book Display Demo'),
	    \books_demo_body).

books_demo_body -->
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
	    \book_viewer(book_opts)
	     ]).


book_opts(provider(google)).   % this is the default anyway
book_opts(id(viewerdiv)).      % id tag for the viewer
book_opts(book_id('ISBN:3540006788')).   % Clocksin & Mellish

