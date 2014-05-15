:- module(weblogdemo, [
	start_server/0,
        weblog_demo/0,
        stop_server/0,
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
   @tbd should the file handlers for css, etc be in here?

*/

:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(http/http_header)).
:- use_module(library(settings)).
% threaded server
:- use_module(library(http/thread_httpd)).
% basic dispatch
:- use_module(library(http/http_dispatch)).
% to set the dialect
:- use_module(library(http/html_write)).
% logging - turns on and gets http_log
:- use_module(library(http/http_log)).
% head - so we can refer to resources
:- use_module(library(http/html_head)).

:- use_module(library(http/http_files)).
:- use_module(library(www_browser)).

:- use_module(weblog(formatting/boxes)).

:- setting(http:port, nonneg, env('PORT', 4050),
    'Port the http server listens to').

%	%%%%%%%%%%%%%%%%%%%%  SERVER CONTROL  %%%%%%%%%%%%%%%%%%%

%%	start_server is nondet
%
%	Starts the weblog demo server.
%	nondet because the server might not start
%
start_server:-
  setting(http:port, Port),
  start_server(Port).

start_server(Port):-
  must_be(between(1000,9999), Port),
  http_server_property(Port, start_time(StartTime)), !,
  print_message(informational, server_running(Port,StartTime)).
start_server(Port):-
	% @tbd for unclear reasons, uncommenting this breaks the Google Maps demo.
	%html_set_options([dialect(xhtml)]),
	http_server(http_dispatch, [port(Port), timeout(3600)]),

  at_halt(stop_server(Port)),

	http_log('Starting weblog demo on port ~w~n', [Port]),
  print_message(informational, server_started(Port)).

%%	weblog_demo is nondet
%
%	Start the server in debug mode and show
%	the index page
%
%
weblog_demo:-
       start_server,
  setting(http:port, Port),
       format(string(S), 'http://127.0.0.1:~w/' , [Port]),
       www_open_url(S).

normal_debug :-
       debug(html_form),
       % @tbd There seem to be no debug messages for this topic?
       debug(http(request)).

%%	stop_server is det
%
%	Stop the web server
%
stop_server :-
  setting(http:port, Port),
  stop_server(Port).

stop_server(Port):-
  must_be(between(1000,9999), Port),
  http_server_property(Port, _), !,
	http_stop_server(Port, []),
  print_message(informational, server_stopped(Port)).
stop_server(_).

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
	    \html_requires(css('demo.css')),
	    h1('Weblog demo page'),
	    \abox('', 'Input', [\demo_item(testform),
			        \demo_item(testcontrols),
			        \demo_item(autocomplete),
				\demo_item(ajaxify)]),
	    \abox('', 'Info', [
			       \demo_item(map),
			       \demo_item(geocoding),
			       \demo_item(buttons),
			       \demo_item(books)]),
	    \abox('', 'Formatting', [
			       \demo_item(wl_table),
		               \demo_item(wl_windows)]),
	    \abox('', 'Debug', [\demo_item(debug_demo),
				\demo_item(stats)]),
	    \abox('', 'Navigation', [\demo_item(accordion),
				    \demo_item(menu)]),
	    \abox('', 'Widgets', [\demo_item(clippy)]),
	    p('This page also happens to demo boxes, which doesn\'t otherwise have a demo page'),
	    p('geohashing doesn\'t have a demo page, but is extensively used in the Impatient Geohasher application, get it on Github'),
	    p('info/stock/crox/croxstock.pl doesn\'t have a demo page.')
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

:- discontiguous demo_label/2.

demo_label(testform, 'Validated Form').
:- ensure_loaded(html_form_demo).

demo_label(testcontrols, 'Control Test').

demo_label(googlemap, 'Google Map').
:- ensure_loaded(google_map_demo).

demo_label(map, 'Maps').
:- ensure_loaded(map_demo).

demo_label(wl_table, 'Table Generation').
:- ensure_loaded(table_demo).

demo_label(wl_windows, 'Windows and popups').
:- ensure_loaded(wl_windows_demo).

demo_label(debug_demo, 'Debugging Tools').
:- ensure_loaded(debug_demo).

demo_label(stats, 'Server Statistics').
:- ensure_loaded(weblog(debug_page/server_stats)).

demo_label(accordion, 'Accordion').
:- ensure_loaded(accordion_demo).

demo_label(buttons, 'Buttons').
:- ensure_loaded(buttons_demo).

demo_label(geocoding, 'Geocoding').
:- ensure_loaded(geocoding_demo).

demo_label(books, 'Books').
:- ensure_loaded(books_demo).

demo_label(clippy, 'Clippy').
:- ensure_loaded(clippy_demo).

demo_label(autocomplete, 'Auto Complete').
:- ensure_loaded(autocomplete_demo).

demo_label(menu, 'Menus').
:- ensure_loaded(menus_demo).

demo_label(ajaxify, 'Ajaxify').
:- ensure_loaded(ajaxify_demo).


% Messages

:- multifile(prolog:message//1).

prolog:message(server_running(Port,StartTime)) -->
  ['The server at port ~d is already in use (start time: '-[Port]],
  time(StartTime),
  [').'].
prolog:message(server_started(Port)) -->
  ['The weblog demo server started on port ~w.'-[Port]].
prolog:message(server_stopped(Port)) -->
  ['The weblog demo server at port ~d has stopped.'-[Port]].

time(Time) -->
  {http_timestamp(Time, Text)},
  [Text].

