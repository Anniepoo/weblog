:- module(hhpvirtualserver, [
	start/0,
        autostart/0,
	server_port/1,
        stop_server/0,
        using_real_services/0,
	using_simulated_services/0,
	set_server_services/1,
	bye/0,
        normal_debug/0

]).


/**   <module> HHP Virtual web server
   This code based on
      Installer for Cogbot

   Architecture:
      This is an http server. It serves pages related to HHPVirtual.
   This module contains preds for overall server control

   @author Anne Ogborn
   @copyright Copyright (c) 2012, University of Houston  All Rights Reserved.
   @license This code governed by the Cogbot New BSD License
*/

% threaded server
:- use_module(library(http/thread_httpd)).
% basic dispatch
:- use_module(library(http/http_dispatch)).
% to set the dialect
:- use_module(library(http/html_write), [html_set_options/1]).
% logging - turns on and gets http_log
:- use_module(library(http/http_log)).

% flag to ensure we only start server once
:- dynamic started/0.

%%   server_port(-Port:int) is det
%
% Returns the number to run the server on
%
% @param Port the port the server should listen on
server_port(11070).

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
	http_log('Starting server on port ~w~n' , [Port]).

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
       set_server_services(simulate),
       start,
       server_port(Port),
       format(string(S), 'http://127.0.0.1:~w/admin/login' , [Port]),
       www_open_url(S).

normal_debug :-
       debug(message),
       debug(addaccounts),
       debug(email),
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

%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	For development, we can use simulated external services
%

%%	 using_simulated_services is nondet
%
%	if succeeds, we're using simulated services
%
:- dynamic using_simulated_services/0.

%%	set_server_services(+Option:member([real, simulate]))
%
%	The server depends on these services
%
%	* sending emails
%	* querying and adding users to opensim
%
%	These services can be real (to run on hhpvirtual.net)
%	or simulated (for development)
%
set_server_services(real) :-
	nodebug(services),
	retractall(using_simulated_services).

set_server_services(simulate) :-
	debug(services),
	retractall(using_simulated_services),
	assert(using_simulated_services).

%%	using_real_services is nondet
%
%	Succeeds iff we're using real services
%
using_real_services :- \+ using_simulated_services.

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
