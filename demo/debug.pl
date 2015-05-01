:- module(debug, [pldoc/0, portray_list_innerds/1]).

/** <module> Debug weblog

Consult this file to bring up **weblog**'s development environment.

To run the **weblog** demo server:
  - Query `query weblog_demo/0`
  - Visit `http://localhost:4040`

To run the SWI-Prolog documentation server (pldoc):
  - Query `pldoc/0`
  - Visit `http://127.0.0.1:4040/help/source`

@copyright Copyright (C) 2012, University of Houston
Released under the LGPL as part of the Weblog project
*/

% This library allows for exploiting the color and attribute facilities
% of most modern terminals by using ANSI escape sequences.
% The Windows console (swipl-win) does not (yet) support ANSI (color) codes.
:- use_module(library(ansi_term)).
:- use_module(library(doc_http)).
:- use_module(library(http/http_path)). % For HTTP location declarations.
:- use_module(library(portray_text)).
:- use_module(library(www_browser)).

:- multifile http:location/3.

http:location(pldoc, root('help/source'), [priority(10)]).

:- doc_server(4040).

% Assumes that lists of integers represent strings
% and shows the characters with the corresponding codes.
:- portray_text(true).

:- dynamic user:portray/1.
:- multifile user:portray/1.

user:portray([H|T]) :-
	write('['),
	portray_list_innerds([H|T]),
	write(']').

%! portray_list_innerds(+List:list) is det.
% Displays a list without ellipsizing it.
% When debugging termerized HTML ellipsizing gets in the way.

portray_list_innerds([]).
portray_list_innerds([H]) :- !,
	print(H).
portray_list_innerds([H|T]) :-
	print(H),
	write(','),
	portray_list_innerds(T).

% Ensure that we print debug messages from weblog.
% This is particularly important for communicating to the user
% in case API keys are missing.
:- debug(weblog).

:- use_module(load).
:- use_module(weblogdemo).

% Re-exported predicates can be used from the top-level interactor.
:- reexport(weblogdemo, [weblog_demo/0, start_server/0, stop_server/0]).

%! pldoc is det.
%	Run the SWI-Prolog documentation server (pldoc) on port 4040
% of localhost and open the root page.

pldoc :-
	doc_server(4040),
	www_open_url('http://127.0.0.1:4040/help/source').

:- multifile prolog:message//1.

prolog:message(weblog_banner) -->
  ['%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n'],
  ['%                                           %\n'],
  ['%    To run the pldoc server query pldoc.   %\n'],
  ['% To run the weblog demo query weblog_demo. %\n'],
  ['%                                           %\n'],
  ['%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'].

:- print_message(banner, weblog_banner).
