:- module(load, []).
/** <module>  Set up server environment
   Weblog

   Copyright (c) 2012, University of Houston
   Which has released it under the terms of the LGPL


*/

user:file_search_path(weblog, '.').
user:file_search_path(weblogtest, '.').

% Turn on the logger
:- ensure_loaded(library(http/http_log)).
% Turn on sessions
:- ensure_loaded(library(http/http_session)).
% Needed for http:location/3, don't remove even if red
:- use_module(library(http/http_path)).

% these are suspicious
http:location(admin, '/admin' , []).
http:location(demo, '/demo', []).





