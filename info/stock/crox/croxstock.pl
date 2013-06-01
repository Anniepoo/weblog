:- module(croxstock, [crstock_stats/4]).
/** <module> Stock market stats (just the DJIA opening) from crox.net

*/
:- use_module(library(http/http_client)).

:- dynamic stock_stat/4.

/**   crstock_stats(+Market:atom, +Stat:atom, +Date:term, -Value:float) is semidet

     @Market must be djia
     @Stat   must be opening
     @Date   is of the form yy-mm-dd where mm, dd, and yy are ints
     @Value  bound to the value of the DJIA opening for that day

     fails if the dow wasn't open, or hasn't opened yet for that day,
     or the server's down.

     Note - while this server is open, the owner notes:

     No technical restrictions, users are expected to use common sense,
     when in doubt, ask.

*/
crstock_stats(djia, opening, YY - MM - DD, _) :-
	stock_stat(djia, opening, YY - MM - DD, error(Stamp)),
	get_time(Now),
	Now < Stamp + 900,
	!,fail.
crstock_stats(djia, opening, YY - MM - DD, Value) :-
	stock_stat(djia, opening, YY - MM - DD, Value),
	number(Value),
	!.
crstock_stats(djia, opening, YY - MM - DD, Value) :-
	debug(crstock, 'getting value for ~d ~d ~d', [YY, MM, DD]),
	(   YY < 100 -> YYYY is YY + 2000 ; YYYY = YY),
	format(atom(URL), 'http://geo.crox.net/djia/~d/~d/~d', [YYYY, MM, DD]),
	http_get(URL, Reply, []),
	atom_number(Reply, Value),  % fails if crox returns 'error'
	asserta(stock_stat(djia, opening, YY - MM - DD, Value)),!.
crstock_stats(djia, opening, YY - MM - DD, _) :-
	get_time(Stamp),
	asserta(stock_stat(djia, opening, YY - MM - DD, error(Stamp))),!,fail.

