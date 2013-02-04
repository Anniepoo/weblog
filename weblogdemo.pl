:- module(weblogdemo, [
	start/0,
        autostart/0,
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

*/

% threaded server
:- use_module(library(http/thread_httpd)).
% basic dispatch
:- use_module(library(http/http_dispatch)).
% to set the dialect
:- use_module(library(http/html_write)).
% logging - turns on and gets http_log
:- use_module(library(http/http_log)).

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
	html_set_options([dialect(xhtml)]),
	format(user_error, 'Starting hhp virtual server\n', []),
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
autostart :-
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
	    p('Some day this will show cool weblog demos')
	     ]).
