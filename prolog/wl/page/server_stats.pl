:- module(
  server_stats,
  [
    http_session_table//0,
    http_server_statistics//0,
    http_server_pool_table//0
  ]
).

/** <module> Server statistics components

@author Jan Wielemaker
@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_stream)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

:- html_meta odd_even_row(+,-,html,?,?).

:- multifile(weblogdemo:label/2).
weblogdemo:label(server_stats, 'server statistics').

:- http_handler(root(server_stats),	server_stats, [id(server_stats)]).

server_stats(_Request) :-
	reply_html_page(title('SWI-Prolog server statistics'),
			[ \html_requires(css('stats.css')),
			  h1(class(wiki), 'Sessions'),
			  \http_session_table,
			  h1(class(wiki), 'Server statistics'),
			  \http_server_statistics,
			  h2(class(wiki), 'Pool statistics'),
			  \http_server_pool_table
			]).


%%	http_session_table//
%
%	HTML component that writes a table of current sessions.

http_session_table -->
	{ findall(S, session(S), Sessions0),
	  sort(Sessions0, Sessions),
	  Sessions \== [], !
	},
	html([ table([ class(block)
		     ],
		     [ tr([th('User'), th('Real Name'),
			   th('On since'), th('Idle'), th('From')])
		     | \sessions(Sessions, 1)
		     ])
	     ]).
http_session_table -->
	html(p('No users logged in')).

%%	session(-Session:s(Idle, User, SessionID, Peer)) is nondet.
%
%	Enumerate all current HTTP sessions.

session(s(Idle, User, SessionID, Peer)) :-
	http_current_session(SessionID, peer(Peer)),
	http_current_session(SessionID, idle(Idle)),
	User = (-).

sessions([], _) --> [].
sessions([H|T], Row) -->
	odd_even_row(Row, Next, \session(H)),
	sessions(T, Next).

session(s(Idle, -, _SessionID, Peer)) -->
	html([td(-), td(-), td(-), td(\idle(Idle)), td(\ip(Peer))]).
session(s(Idle, User, _SessionID, Peer)) -->
	{  RealName = '?',
	   OnSince = 0
	},
	html([td(User), td(RealName), td(\date(OnSince)), td(\idle(Idle)), td(\ip(Peer))]).

idle(Time) -->
	{ Secs is round(Time),
	  Min is Secs // 60,
	  Sec is Secs mod 60
	},
	html('~`0t~d~2|:~`0t~d~5|'-[Min, Sec]).

date(Date) -->
	{ format_time(string(S), '%+', Date)
	},
	html(S).

ip(ip(A,B,C,D)) --> !,
	html('~d.~d.~d.~d'-[A,B,C,D]).
ip(IP) -->
	html('~w'-[IP]).


%%	http_server_statistics//
%
%	HTML component showing statistics on the HTTP server

http_server_statistics -->
	{ findall(Port-ID, http_current_worker(Port, ID), Workers),
	  group_pairs_by_key(Workers, Servers)
	},
	html([ table([ class(block)
		     ],
		     [ \servers_stats(Servers)
		     ])
	     ]).

servers_stats([]) --> [].
servers_stats([H|T]) -->
	server_stats(H), servers_stats(T).

:- if(catch(statistics(process_cputime, _),_,fail)).
cputime(CPU) :- statistics(process_cputime, CPU).
:- else.
cputime(CPU) :- statistics(cputime, CPU).
:- endif.

server_stats(Port-Workers) -->
	{ length(Workers, NWorkers),
	  http_server_property(Port, start_time(StartTime)),
	  format_time(string(ST), '%+', StartTime),
	  cputime(CPU),
	  statistics(heapused, Heap)
	},
	html([ \server_stat('Port:', Port, odd),
	       \server_stat('Started:', ST, even),
	       \server_stat('Total CPU usage:', [\n('~2f',CPU), ' seconds'], odd),
	       \server_stat('Heap memory:', [ \n(human,Heap), ' bytes' ], even),
	       \request_statistics,
	       \server_stat('# worker threads:', NWorkers, odd),
	       tr(th(colspan(6), 'Statistics by worker')),
	       tr([ th('Thread'),
		    th('CPU'),
		    th(''),
		    th('Local'),
		    th('Global'),
		    th('Trail')
		  ]),
	       \http_workers(Workers, odd)
	     ]).

server_stat(Name, Value, OE) -->
	html(tr(class(OE),
		[ th([class(p_name), colspan(3)], Name),
		  td([class(value),  colspan(3)], Value)
		])).


request_statistics -->
	{ cgi_statistics(requests(Count)),
	  cgi_statistics(bytes_sent(Sent))
	},
	server_stat('Requests processed:', \n(human, Count), odd),
	server_stat('Bytes sent:', \n(human, Sent), even).


http_workers([], _) -->
	[].
http_workers([H|T], OE) -->
	{ odd_even(OE, OE2) },
	http_worker(H, OE),
	http_workers(T, OE2).

http_worker(H, OE) -->
	{ thread_statistics(H, locallimit, LL),
	  thread_statistics(H, globallimit, GL),
	  thread_statistics(H, traillimit, TL),
	  thread_statistics(H, localused, LU),
	  thread_statistics(H, globalused, GU),
	  thread_statistics(H, trailused, TU),
	  thread_statistics(H, cputime, CPU)
	},
	html([ tr(class(OE),
		  [ td(rowspan(2), H),
		    \nc('~3f', CPU, [rowspan(2)]),
		    th('In use'),
		    \nc(human, LU),
		    \nc(human, GU),
		    \nc(human, TU)
		  ]),
	       tr(class(OE),
		  [ th('Limit'),
		    \nc(human, LL),
		    \nc(human, GL),
		    \nc(human, TL)
		  ])
	     ]).

odd_even(even, odd).
odd_even(odd, even).


		 /*******************************
		 *	      POOLS		*
		 *******************************/

%%	http_server_pool_table//
%
%	Display table with statistics on thread-pools.

http_server_pool_table -->
	{ findall(Pool, current_thread_pool(Pool), Pools),
	  sort(Pools, Sorted)
	},
	html(table([ id('http-server-pool'),
		     class(block)
		   ],
		   [ tr([th('Name'), th('Running'), th('Size'), th('Waiting'), th('Backlog')])
		   | \server_pools(Sorted, 1)
		   ])).

server_pools([], _) --> [].
server_pools([H|T], Row) -->
	odd_even_row(Row, Next, \server_pool(H)),
	server_pools(T, Next).

server_pool(Pool) -->
	{ findall(P, thread_pool_property(Pool, P), List),
	  memberchk(size(Size), List),
	  memberchk(running(Running), List),
	  memberchk(backlog(Waiting), List),
	  memberchk(options(Options), List),
	  option(backlog(MaxBackLog), Options, infinite)
	},
	html([ th(class(p_name), Pool),
	       \nc(human, Running),
	       \nc(human, Size),
	       \nc(human, Waiting),
	       \nc(human, MaxBackLog)
	     ]).


		 /*******************************
		 *	       BASICS		*
		 *******************************/

%%	n(+Format, +Value)//
%
%	HTML component to emit a number.
%
%	@see nc//2 for details.

n(Fmt, Value) -->
	{ number_html(Fmt, Value, HTML) },
	html(HTML).

number_html(human, Value, HTML) :-
	integer(Value), !,
	human_count(Value, HTML).
number_html(Fmt, Value, HTML) :-
	number(Value), !,
	HTML = Fmt-[Value].
number_html(_, Value, '~p'-[Value]).


human_count(Number, HTML) :-
	Number < 1024, !,
	HTML = '~d'-[Number].
human_count(Number, HTML) :-
	Number < 1024*1024, !,
	KB is Number/1024,
	digits(KB, N),
	HTML = '~*fK'-[N, KB].
human_count(Number, HTML) :-
	Number < 1024*1024*1024, !,
	MB is Number/(1024*1024),
	digits(MB, N),
	HTML = '~*fM'-[N, MB].
human_count(Number, HTML) :-
	TB is Number/(1024*1024*1024),
	digits(TB, N),
	HTML = '~*fG'-[N, TB].

digits(Count, N) :-
	(   Count < 100
	->  N = 1
	;   N = 0
	).


%%	nc(+Format, +Value)// is det.
%%	nc(+Format, +Value, +Options)// is det.
%
%	Numeric  cell.  The  value  is    formatted   using  Format  and
%	right-aligned in a table cell (td).
%
%	@param	Format is a (numeric) format as described by format/2 or
%		the constant =human=.  _Human_ formatting applies to
%		integers and prints then in abreviated (K,M,T) form,
%		e.g., 4.5M for 4.5 million.
%	@param	Options is passed as attributed to the =td= element.
%		Default alignment is =right=.

nc(Fmt, Value) -->
	nc(Fmt, Value, []).

nc(Fmt, Value, Options) -->
	{ class(Value, Class),
	  merge_options(Options,
			[ align(right),
			  class(Class)
			], Opts),
	  number_html(Fmt, Value, HTML)
	},
	html(td(Opts, HTML)).

class(Value, Class) :-
	(   integer(Value)
	->  Class = int
	;   float(Value)
	->  Class = float
	;   Class = value
	).

%%	odd_even_row(+Row, -Next, :Content)//
%
%	Create odd/even alternating table rows from a DCG.

odd_even_row(Row, Next, Content) -->
	{ (   Row mod 2 =:= 0
	  ->  Class = even
	  ;   Class = odd
	  ),
	  Next is Row+1
	},
	html(tr(class(Class), Content)).
