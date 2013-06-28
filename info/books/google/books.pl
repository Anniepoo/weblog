:- module(books, [book_viewer//1]).
/** <module> Generic interface for online book viewer/review widgets

   Hoping we get extended mime support in resources so we can finish
   this

*/

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

:- meta_predicate book_viewer(1, ?, ?).
:- style_check(-atom).

book_viewer(Generator) -->
	{
	   (   call(Generator, id(ID)) ; ID = bookViewerCanvas ),
	   call(Generator, book_id(Book))
	},
	html([
	    \html_requires('https://www.google.com/jsapi'),
	    \html_post(head,
		       [script([type='text/javascript'], [
'      google.load("books", "0");

      function initialize() {
        var ~wviewer = new google.books.DefaultViewer(document.getElementById(\'~w\'));'
	   -[ID,ID],
'        ~wviewer.load(\'~w\');
      }

      google.setOnLoadCallback(initialize);'-[ID, Book]
							 ])]),
	    div([id=ID], [&(nbsp)])
	     ]).
