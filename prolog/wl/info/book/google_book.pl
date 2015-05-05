:- module(
  google_book,
  [
    google_book_viewer//1 % :Generator
  ]
).

/** <module> Google Book

Support for Google Books services.

At the time of writing Google does not require an API key for these services.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).

:- meta_predicate google_book_viewer(1,?,?).

:- html_resource(
  'https://www.google.com/jsapi',
  [mime_type(text/javascript)]
).



%! google_book_viewer(:Generator)// is det

google_book_viewer(Generator) -->
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
