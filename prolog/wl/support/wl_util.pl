:- module(
  wl_util,
  [
    js_friendly_html/2, % +HTML
                        % -FriendlyHTML
    wl_opts/2
  ]
).

/** <module> Weblog utilities

Misc utilities useful with weblog

This module should not become a kitchen sink.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
*/

:- use_module(library(http/html_write)).

:- html_meta js_friendly_html(html, -).



%! js_friendly_html(+HTML, -FriendlyHTML) is det.
% Given termerized HTML, return an atom with
% the normal html in it, after removing any newlines and
% escaping single and double quotes.
%
% It is common to stuff HTML into a JavaScript string.
% This predicate handles JavaScript's lack of heredoc syntax.

js_friendly_html(HTML, FriendlyHTML) :-
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



%! wl_opts(+MatchList:list, +Template:term) is nondet.
%! wl_opts(+MatchList:atom, +Template:term) is nondet.
%	Succeeds if MatchList unifies with Template, or MatchList
%	is a list with a member that unifies with template. This
%	is a convenience method when all the generator predicate
%	is is facts.

wl_opts(Match, Match).
wl_opts(MatchList, Template) :-
	member(Template, MatchList).
