/* Load file

To run the **WebLog** demo server:
  - Query `query weblog_demo/0`
  - Visit `http://localhost:4040`

@copyright Copyright (C) 2012, University of Houston.
Released under the LGPL as part of the WebLog project.
*/

% This library allows for exploiting the color and attribute facilities
% of most modern terminals by using ANSI escape sequences.
% The Windows console (swipl-win) does not (yet) support ANSI (color) codes.
:- use_module(library(ansi_term)).

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

init_weblog_demo:-
  % The use of `file_search_path(weblogtest, .)` is unreliable,
  % since the meaning of point depends on the directory from
  % which swipl was called.
  source_file(init_weblog_demo, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(wl_demo, ThisDirectory)),
  assert(user:file_search_path(library, wl_demo('../prolog'))),
  assert(user:file_search_path(css, library(wl/resource/css))),
  assert(user:file_search_path(js, library(wl/resource/js))),
  assert(user:file_search_path(icon, library(wl/resource/icon))).
:- init_weblog_demo.

:- use_module(weblog_demo).

:- multifile prolog:message//1.

prolog:message(weblog_banner) -->
  ['%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n'],
  ['%                                           %\n'],
  ['%    To run the pldoc server query pldoc.   %\n'],
  ['% To run the WebLog demo query weblog_demo. %\n'],
  ['%                                           %\n'],
  ['%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'].

:- print_message(banner, weblog_banner).
