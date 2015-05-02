:- module(load, []).

/** <module> Load file

Sets up the server environment for **weblog**.

Copyright (c) 2012, University of Houston.
Which has released it under the terms of the LGPL
*/

:- use_module(library(http/http_log)). % HTTP logging.
:- use_module(library(http/http_session)). % HTTP sessions.

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

:- initialization(init_weblog_demo).

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
