:- module(weblogdemo, [
	start/0,
        weblog_demo/0,
	server_port/1,
        stop_server/0,
	bye/0,
        normal_debug/0

]).


/**   <module> Web Log Demo
   This code based on
      Installer for Cogbot
      HHP Virtual Web Application

   Architecture:
      This is an http server. It serves a demo of weblog.
   This module contains preds for overall server control

   @author Anne Ogborn
   @copyright Copyright (c) 2012, University of Houston
   @license This code governed by the Cogbot New BSD License
   @tbd figure out why google maps isn't happy with xhtml

*/

% threaded server
:- use_module(library(http/thread_httpd)).
% basic dispatch
:- use_module(library(http/http_dispatch)).
% to set the dialect
:- use_module(library(http/html_write)).
% logging - turns on and gets http_log
:- use_module(library(http/http_log)).
% needed to handle params in some of the demo pages
:- use_module(library(http/http_parameters)).
% needed by tables demo
:- use_module(library(clpfd)).

:- ensure_loaded(weblog(debug_page/debug_page)).
:- ensure_loaded(weblogtest(html_form/html_form_test)).
:- use_module(weblog(info/google/maps/gmap)).
:- use_module(weblog(formatting/basics)).
:- use_module(weblog(formatting/tables)).

% flag to ensure we only start server once
:- dynamic started/0.

%%   server_port(-Port:int) is det
%
% Returns the number to run the server on
%
% @param Port the port the server should listen on
server_port(4050).

%	%%%%%%%%%%%%%%%%%%%%  SERVER CONTROL  %%%%%%%%%%%%%%%%%%%

%%	start is nondet
%
%	Starts the server
%	nondet because the server might not start
%
start:-
	started,!,
	server_port(Port),
	format(user_error, 'Already running - browse http://127.0.0.1:~w/\n', [Port]).

start:-
	% for unclear reasons, uncommenting this breaks the google maps
	% demo
%	html_set_options([dialect(xhtml)]),
	format(user_error, 'Starting weblog demo server\n', []),
	server_port(Port),
	http_server(http_dispatch, [port(Port), timeout(3600)]),
	assert(started),
	http_log('Starting weblog demo on port ~w~n' , [Port]).

%%	autostart is nondet
%
%	Start the server in debug mode and show
%	the admin login page
%	debug mode means
%
% * various debug messages are turned on
% * services are set to 'simulate'
%
weblog_demo:-
       start,
       server_port(Port),
       format(string(S), 'http://127.0.0.1:~w/' , [Port]),
       www_open_url(S).

normal_debug :-
       debug(html_form),
       debug(http(request)).

%%	stop_server is det
%
%	Stop the web server
%
stop_server :-
	server_port(Port),
	http_stop_server(Port, []),
	format(user_error, 'Server halted on port ~n', [Port]).

%%      bye is det
%
%  shut down server and exit
%
bye :-
	stop_server,
	halt.

%
%  No other good place for this, so it's here
%
:- http_handler(root(.) , redir_to_index,
		[id(indexroot)]).

%%	redir_to_index(+Request:http_request) is det
%
%	handle bare domain request by redirection to index
%
%	@param Request the HTTP request as the usual list format
%
redir_to_index(Request) :-
	http_redirect(moved_temporary, location_by_id(index), Request).

:- http_handler(root('index.htm'), index_page , [id(index)]).

index_page(_Request) :-
	reply_html_page(
	    title('Weblog Demo'),
	    [
	    h1('Weblog demo page'),
	    \demo_item(testform),
	    \demo_item(googlemap),
	    \demo_item(table)
	    ]).

%%	demo_item(+Item:location_id)
%
%	Pass the location id and it generates the demo badge
%
demo_item(Item) -->
	{
	    demo_label(Item, Label)
	    ;
	    atom_concat('Oops, No Label For ', Item, Label)
	},
	html([
	    p(a(href=location_by_id(Item), ['Demo ', Label]))
	     ]).

%
%         it's ugly having all this in one file, but
%         I haven't decided how to architect breaking it up yet
%         so bear with me here

demo_label(testform, 'Validated Form').
demo_label(googlemap, 'Google Map').
demo_label(table, 'Table Generation').

:- http_handler(root(googlemap), google_map_handler, [id(googlemap)]).

google_map_handler(_Request) :-
	reply_html_page(
	    title('Google Map Demo'),
	    [
	    h1('Google Map demo page'),
	     p('Weblog contributors'),
	    \gmap([
		      point(37.482214,-122.176552),     % Annie
		      point(37.969368,23.732979),       % Acropolis
		      point(52.334434,4.863596),        % VNU
		      point(29.720576,-95.34296)        % Univ. of Houston
		       ])
	    ]).

:- http_handler(root(table), table_handler, [id(table)]).

table_handler(_Request) :-
	reply_html_page(
	    title('Table Demo'),
	    [
	     style(
'tr.even td, tr.even {
	     background-color: #aaaaff;
	    }'),
	     h1('Table Demo'),
	     p('Table from nested list data'),
	     \wl_direct_table([
		 head(['Name', 'Quiz1', 'Quiz2', 'Midterm', 'Final']),
		 ['Abigail Ames', 73, 84, 92, 87],
		 ['Bob Burns', 23, 45, 77, 45],
		 ['Charlie Clark', 99, 100, 89, 94]
		      ]),
	     hr([]),
	     p('Table From Facts'),
	     \wl_table(grades_table_cells, []),
	     hr([]),
	     p('Table From Facts with Column Names'),
	     \wl_table(grades_table_cells, [header(weblogdemo:grade_labels)]),
	     hr([]),
	     p('Table From Facts without Header'),
	     \wl_table(grades_table_cells, [header(none)])
	    ]).

grade_labels(name, 'Student').
grade_labels(quiz1, 'Quiz 1').
grade_labels(quiz2, 'Quiz 2').
grade_labels(midterm, 'Midterm').
grade_labels(final, 'Final').

%  Note that we're styling the text before returning
%
%  NOTE - subtlety here. The order of the answers is important,
%  which is non logical.  The -> is needed to avoid leaving
%  a choice point in a bad place.
%
grades_table_cells(Name, name, b(Name)) :- grade(Name, _, _).
grades_table_cells(Name, Assignment, RetValue) :-
	grade(Name, Assignment, Value),
	(   Value < 60
	->  RetValue = em(Value)
	;   RetValue = Value
	).

grade('Arnie Adams', quiz1, 45).
grade('Arnie Adams', quiz2, 65).
grade('Arnie Adams', midterm, 85).
grade('Arnie Adams', final, 45).

grade('Brenda Burns', quiz1, 83).
grade('Brenda Burns', quiz2, 85).
grade('Brenda Burns', midterm, 95).
grade('Brenda Burns', final, 95).

grade('Cindy Cameo', quiz1, 40).
%   missing value
grade('Cindy Cameo', midterm, 39).
grade('Cindy Cameo', final, 29).

grade('Dwight Dangerman', quiz1, 78).
grade('Dwight Dangerman', quiz2, 98).
grade('Dwight Dangerman', midterm, 85).
grade('Dwight Dangerman', final, 90).






