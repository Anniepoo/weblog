:- module(autocomplete_demo, []).

/** <module> Auto-complete demo

Generates an HTML demo page for the auto-completion widget.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(wl/form/wl_autocomplete)).

:- http_handler(root(autocomplete), autocomplete_demo, [id(autocomplete)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(autocomplete, 'auto-completion').



autocomplete_demo(_):-
  reply_html_page(
    wl_demo,
    title('Autocomplete Demo'),
    \autocomplete_demo_body
  ).

autocomplete_demo_body -->
	html([
	    h1('Autocomplete Demo'),
	    form([action='#'],[
		  p('All data is preloaded with the page'),
		  p([
		     label([for=athingy], 'Normal: '),
		     \wl_autocomplete(autocomplete_opts)]),
		  p('This demonstrates ignoring accented chars.'),
		  p([
		     label([for=accentthingy], 'Accents: '),
		     \wl_autocomplete(accent_opts)]),
		  p('This data comes from the server'),
		  p([
		      label([for=ajaxthingy], 'Ajax: '),
		     \wl_autocomplete(ajax_opts)])
		 ])
	]).

ajax_opts(id(ajaxthingy)) :- !.
ajax_opts(ajax).
ajax_opts(choice(Term, AX)) :-
	autocomplete_opts(choice(AX)),
	atom_codes(AX, CX),
	atom_codes(Term, CTerm),
	append(CTerm, _, CX).

accent_opts(accents).
accent_opts(id(accentthingy)) :- !.
accent_opts(X) :- autocomplete_opts(X).

autocomplete_opts(id(athingy)).
autocomplete_opts(choice('ActionScript')).
autocomplete_opts(choice('AppleScript')).
autocomplete_opts(choice('Asp')).
autocomplete_opts(choice('BASIC')).
autocomplete_opts(choice('C')).
autocomplete_opts(choice('C++')).
autocomplete_opts(choice('Clojure')).
autocomplete_opts(choice('COBOL')).
autocomplete_opts(choice('ColdFusion')).
autocomplete_opts(choice('Erlang')).
autocomplete_opts(choice('Fortran')).
autocomplete_opts(choice('Groovy')).
autocomplete_opts(choice('Haskell')).
autocomplete_opts(choice('Java')).
autocomplete_opts(choice('JavaScript')).
autocomplete_opts(choice('Lisp')).
autocomplete_opts(choice('Perl')).
autocomplete_opts(choice('PHP')).
autocomplete_opts(choice('Python')).
autocomplete_opts(choice('Ruby')).
autocomplete_opts(choice('Scala')).
autocomplete_opts(choice('Scheme')).

