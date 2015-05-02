/* Load file

To run the **weblog** demo server:
  - Query `query weblog_demo/0`
  - Visit `http://localhost:4040`

@copyright Copyright (C) 2012, University of Houston.
Released under the LGPL as part of the Weblog project.
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
  assert(user:file_search_path(weblogtest, ThisDirectory)),
  assert(user:file_search_path(weblog, weblogtest('../prolog'))),
  assert(user:file_search_path(css, weblog('static/css'))),
  assert(user:file_search_path(js, weblog('static/js'))),
  assert(user:file_search_path(icons, weblog('static/icons'))).
:- init_weblog_demo.

:- use_module(weblogdemo).

:- multifile prolog:message//1.

prolog:message(weblog_banner) -->
  ['%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n'],
  ['%                                           %\n'],
  ['%    To run the pldoc server query pldoc.   %\n'],
  ['% To run the weblog demo query weblog_demo. %\n'],
  ['%                                           %\n'],
  ['%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'].

:- print_message(banner, weblog_banner).
