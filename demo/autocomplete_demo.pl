:- module(autocomplete_demo, []).
/** <module> Demo page for autocomplete widget

*/
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(weblog(html_form/autocomplete)).

:- http_handler(root(autocomplete), autocomplete_demo_page, [id(autocomplete)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(autocomplete, 'Auto Complete').

autocomplete_demo_page(_Request) :-
	reply_html_page(
    weblog_demo,
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
		     \autocomplete(autocomplete_opts)]),
		  p('This demonstrates ignoring accented chars.'),
		  p([
		     label([for=accentthingy], 'Accents: '),
		     \autocomplete(accent_opts)]),
		  p('This data comes from the server'),
		  p([
		      label([for=ajaxthingy], 'Ajax: '),
		     \autocomplete(ajax_opts)])
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

