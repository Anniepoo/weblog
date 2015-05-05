/* Debug file

Load the **weblog** demo together with some developer tools
and documentation.

See load.pl for starting the demo.

To run the SWI-Prolog documentation server (pldoc):
  - Query `pldoc/0`
  - Visit `http://127.0.0.1:4040/help/source`

@copyright Copyright (C) 2012, University of Houston.
Released under the LGPL as part of the Weblog project.
*/

:- use_module(library(portray_text)).

:- use_module(load_pldoc_server).

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
:- debug(wl).

:- [load].
