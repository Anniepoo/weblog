:- module(javascript_utils, [javascript_friendly_html/2]).
/** <module> Javascript handling utilities

   6/29/2013 renamed to avoid colliding with library javascript

*/
:- use_module(library(http/html_write)).

:- html_meta javascript_friendly_html(html, -).

/** javascript_friendly_html(+HTML, -FriendlyHTML)

   Given termerized HTML, return an atom with
   the normal html in it, after removing any newlines and
   escaping single and double quotes.

   It's fairly common to stuff html into a javascript string. This
   bit handles javascript's lack of heredoc syntax.
   */

javascript_friendly_html(HTML, FriendlyHTML) :-
	phrase(html(HTML), Tokens),
	with_output_to(codes(RawCodes), print_html(Tokens)),
	js_friendly(RawCodes, Codes),
	atom_codes(FriendlyHTML, Codes).

js_friendly([], []).
% Line feeds are skipped.
js_friendly([10 | RT] , T) :-
	js_friendly(RT, T).
% Carriage returns are skipped.
js_friendly([13 | RT], T) :-
	js_friendly(RT, T).
% Double quotes are escaped by backslashes.
js_friendly([34 | RT], [92 , 34 | T]) :-
	js_friendly(RT, T).
% Apostrophes are escaped by backslashes.
js_friendly([39 | RT], [92 , 39 | T]) :-
	js_friendly(RT, T).
js_friendly([RH | RT], [RH | T]) :-
	js_friendly(RT, T).

