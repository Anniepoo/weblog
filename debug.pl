:- module(debug, [pldoc/0, portray_list_innerds/1]).
/**   <module> Consult me to bring up normal dev environment.

   To run the server, query autostart/0.
   To run the pldoc server, query pldoc/0, which starts the
   pldoc server on http://127.0.0.1:4040/help/source

   @copyright Copyright (C) 2012, University of Houston
   Released under the LGPL as part of the Weblog project
*/

% Needed for http:location/3, don't remove even if red!!!
:- use_module(library(http/http_path)).

:- multifile http:location/3.

http:location(pldoc, root('help/source'), [priority(10)]).

:- doc_server(4040).
% makes codes style strings be "abc" instead of numbers
:- portray_text(true).
% makes string style strings be `abc` instead of "abc"
% which is ez to confuse with codes
:- set_prolog_flag(backquoted_string, true).
% more reasonable default for how many items to print before ellipsizing
:- set_prolog_flag(toplevel_print_options,
	[backquoted_string(true), max_depth(9999),
	 portray(true), spacing(next_argument)]).
:- set_prolog_flag(debugger_print_options,
	[backquoted_string(true), max_depth(9999),
	 portray(true), spacing(next_argument)]).
user:portray([H|T]) :-
	write('['),
	portray_list_innerds([H|T]),
	write(']').

portray_list_innerds([]).
portray_list_innerds([H]) :-
	print(H).
portray_list_innerds([H|T]) :-
	print(H),
	write(','),
	portray_list_innerds(T).


:- ensure_loaded(load).
:- ensure_loaded(weblog(debug_page/debug_page)).
:- ensure_loaded(weblogtest(html_form/html_form_test)).

:- ensure_loaded(weblogdemo).

%%	pldoc is det
%
%	Run the pldoc server on 4040 and open the root page
%
pldoc :-
	doc_server(4040),
	www_open_url('http://127.0.0.1:4040/help/source').



:- writeln('To run the pldoc server query pldoc').
:- writeln('To run the weblogdemo query weblog_demo').
