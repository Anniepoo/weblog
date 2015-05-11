:- module(
  weblog_demo,
  [
    weblog_demo/0
  ]
).

/** <module> WebLog Demo

This code is based on:
  * Installer for Cogbot
  * HHP Virtual Web Application

Architecture:
   This is an http server. It serves a demo of WebLog.
This module contains preds for overall server control

@author Anne Ogborn
@author Wouter Beek
@copyright Copyright (c) 2012, University of Houston
@license This code governed by the Cogbot New BSD License
@tbd figure out why google maps isn't happy with xhtml
@tbd should the file handlers for css, etc be in here?
@version 2013-2015
*/

:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_path)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(settings)).
:- use_module(library(uri)).
:- use_module(library(wl/page/server_stats)).
:- use_module(library(wl/format/wl_box)).
:- use_module(library(www_browser)).

:- use_module(accordion_demo).
:- use_module(ajaxify_demo).
:- use_module(autocomplete_demo).
:- use_module(book_demo).
:- use_module(clippy_demo).
:- use_module(collection_demo).
:- use_module(debug_demo).
:- use_module(font_demo).
:- use_module(geocoding_demo).
:- use_module(html_catch_demo).
:- use_module(html_form_demo).
:- use_module(link_demo).
:- use_module(list_demo).
:- use_module(listbox_demo).
:- use_module(map_demo).
:- use_module(menu_demo).
:- use_module(radio_demo).
:- use_module(social_buttons_demo).
:- use_module(table_demo).
:- use_module(window_demo).
:- use_module(library(wl/format/wl_link)).
:- use_module(library(wl/info/font/google_font)).

:- html_resource(css('demo.css'), []).

% A WebLog demo module must add a clause to this predicate
% in order to appear on the WebLog demo page.
:- multifile(weblogdemo:label/2).

:- http_handler(root(.) , redir_to_index, [id(indexroot)]).
:- http_handler(root('index.htm'), index_page , [id(index)]).

:- setting(
  http:port, nonneg, env('PORT', 4050),
  'Port the HTTP server listens to.'
).

:- multifile(user:head//2).
:- multifile(user:body//2).

user:head(wl_demo, Content) -->
  {http_absolute_location(icon('favicon.ico'), Icon, [])},
  html(
    head([
      \wl_favicon(Icon),
      \google_font('Share Tech Mono'),
      \html_requires(css('demo.css')),
      Content
    ])
  ).

user:body(wl_demo, Content) -->
  html(body([nav(a(href='index.htm','Back to index')),Content])).



%! weblog_demo is det.
%	Start the server in debug mode and show	the index page.

weblog_demo:-
  start_server(Port),
  uri_authority_components(Authority, uri_authority(_,_,'127.0.0.1',Port)),
  uri_components(Uri, uri_components(http,Authority,_,_,_)),
  www_open_url(Uri).

%! redir_to_index(+Request:list) is det.
%	Handle bare domain request by redirection to the demo index.
%
%	@param Request The HTTP request in the usual list format.

redir_to_index(Request):-
	http_redirect(moved_temporary, location_by_id(index), Request).

%! index_page(+Request:list) is det.
% Replies with a HTML page enumerating the **WebLog** demos.

index_page(_):-
  reply_html_page(
    wl_demo,
    title('WebLog Demo'),
    [
      h1('WebLog demo page'),
      \abox('Input', [
        \demo_item(ajaxify),
        \demo_item(form),
        \demo_item(autocomplete),
        \demo_item(radio)
      ]),
      \abox('Info', [
        \demo_item(book),
        \demo_item(geocoding),
        \demo_item(map),
        \demo_item(social_button)
      ]),
      \abox('Formatting', [
        \demo_item(collection),
        \demo_item(font),
        \demo_item(html_catch),
        \demo_item(link),
        \demo_item(list),
        \demo_item(listbox),
        \demo_item(table),
        \demo_item(window)
      ]),
      \abox('Debug', [
        \demo_item(debug_demo),
        \demo_item(server_stats)
      ]),
      \abox('Navigation', [
        \demo_item(accordion),
        \demo_item(menu)
      ]),
      \abox('Widgets', [
        \demo_item(clippy)
      ]),
      p('This page also happens to demo boxes, which doesn\'t otherwise have a demo page'),
      p('geohashing doesn\'t have a demo page, but is extensively used in the Impatient Geohasher application, get it on Github'),
      p('info/stock/crox/croxstock.pl doesn\'t have a demo page.')
    ]
  ).

%! demo_item(+Item:atom)
% Pass the location id and it generates the demo badge

demo_item(Item) -->
  {display_label(Item, Label)},
  html(p(a(href=location_by_id(Item), ['Demo ',Label]))).

%! display_label(+Item:atom, -Label:atom) is det.

display_label(Item, Label):-
  weblogdemo:label(Item, Label), !.
display_label(Item, Label):-
  atom_concat('Oops, no display label found for ', Item, Label).



% HELPERS %

%! start_server(+Port:between(1000,9999)) is det.

start_server(Port):-
  var(Port), !,
  setting(http:port, Port),
  start_server(Port).
start_server(Port):-
  must_be(between(1000,9999), Port),
  (   http_server_property(Port, start_time(StartTime))
  ->  print_message(warning, server_running(Port,StartTime))
  ;   % @tbd Uncommenting the following breaks the Google Maps demo.
      %html_set_options([dialect(xhtml)]),
      http_server(http_dispatch, [port(Port),timeout(3600)]),
      at_halt(stop_server(Port)),
      http_log('Starting WebLog demo on port ~w~n', [Port]),
      print_message(informational, server_started(Port))
  ).


%! stop_server(+Port:between(1000,9999)) is det.
% Stops the Web server.

stop_server(Port):-
  (   must_be(between(1000,9999), Port),
      http_server_property(Port, _)
  ->  http_stop_server(Port, []),
      print_message(informational, server_stopped(Port))
  ;   true
  ).



% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(server_running(Port,StartTime)) -->
  ['The server at port ~d is already in use (start time: '-[Port]],
  time(StartTime),
  [').'].
prolog:message(server_started(Port)) -->
  ['The WebLog demo server started on port ~w.'-[Port]].
prolog:message(server_stopped(Port)) -->
  ['The WebLog demo server at port ~d has stopped.'-[Port]].

time(Time) -->
  {http_timestamp(Time, Text)},
  [Text].
