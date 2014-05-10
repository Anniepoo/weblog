:- module(utils, [wl_opts/2]).
/** <module> Misc utilities useful with weblog

it's important that this not become a kitchen sink

*/

%%	wl_opts(+MatchList:list, +Template:term) is nondet
%	wl_opts(+MatchList:atom, +Template:term) is nondet
%
%	succeeds if MatchList unifies with Template, or MatchList
%	is a list with a member that unifies with template. This
%	is a convenience method when all the generator predicate
%	is is facts.
%
wl_opts(Match, Match).
wl_opts(MatchList, Template) :-
	member(Template, MatchList).
