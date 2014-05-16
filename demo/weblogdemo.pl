:- module(weblogdemo, [
  start_server/0,
  stop_server/0,
        weblog_demo/0,
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
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_log)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(settings)).
:- use_module(library(uri)).
:- use_module(library(www_browser)).

% Load the modules that are included in the weblog demo.
:- use_module(accordion_demo).
:- use_module(ajaxify_demo).
:- use_module(autocomplete_demo).
:- use_module(books_demo).
:- use_module(buttons_demo).
:- use_module(clippy_demo).
:- use_module(debug_demo).
:- use_module(geocoding_demo).
:- use_module(html_form_demo).
:- use_module(google_map_demo).
:- use_module(map_demo).
:- use_module(menus_demo).
:- use_module(table_demo).
:- use_module(testcontrols_demo).
:- use_module(wl_windows_demo).

:- use_module(weblog(debug_page/server_stats)).
:- use_module(weblog(formatting/boxes)).

% A weblog demo module must add a clause to this predicate
% in order to appear on the weblog demo page.
:- multifile(weblogdemo:label/2).

:- setting(http:port, nonneg, env('PORT', 4050),
    'Port the HTTP server listens to.').

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
  setting(http:port, Port),
  start_server(Port),
  uri_authority_components(Authority, uri_authority(_,_,'127.0.0.1',Port)),
  uri_components(Url, uri_components(http,Authority,_,_,_)),
  www_open_url(Url).

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
	    \abox('Input', [\demo_item(testform),
			        \demo_item(testcontrols),
			        \demo_item(autocomplete),
				\demo_item(ajaxify)]),
	    \abox('Info', [
			       \demo_item(map),
			       \demo_item(geocoding),
			       \demo_item(buttons),
			       \demo_item(books)]),
	    \abox('Formatting', [
			       \demo_item(wl_table),
		               \demo_item(wl_windows)]),
	    \abox('Debug', [\demo_item(debug_demo),
				\demo_item(server_stats)]),
	    \abox('Navigation', [\demo_item(accordion),
				    \demo_item(menu)]),
	    \abox('Widgets', [\demo_item(clippy)]),
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
	    weblogdemo:label(Item, Label)
	    ;
	    atom_concat('Oops, No Label For ', Item, Label)
	},
	html([
	    p(a(href=location_by_id(Item), ['Demo ', Label]))
	     ]).


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

