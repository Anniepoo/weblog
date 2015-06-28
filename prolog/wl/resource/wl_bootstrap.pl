:- module(wl_bootstrap, []).

/** <module> Weblog version of Bootstrap

Defines the HTML resources for including Bootstrap into a Web page.

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2015
*/

:- use_module(library(debug)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(wl/resource/wl_jquery)).

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

user:file_search_path(css, library(wl/resource/css)).
user:file_search_path(js, library(wl/resource/js)).

:- http_handler(css(.), serve_files_in_directory(css), [prefix]).
:- http_handler(js(.), serve_files_in_directory(js), [prefix]).

:- if(debugging(css(bootstrap))).
  :- html_resource(
    css(bootstrap),
    [requires([css('bootstrap-3.3.5.css')]),virtual(true)]
  ).
:- else.
  :- html_resource(
    css(bootstrap),
    [requires([css('bootstrap-3.3.5.min.css')]),virtual(true)]
  ).
:- endif.

:- if(debugging(css('bootstrap-theme'))).
  :- html_resource(
    css('bootstrap-theme'),
    [requires([css('bootstrap-theme-3.3.5.css')]),virtual(true)]
  ).
:- else.
  :- html_resource(
    css('bootstrap-theme'),
    [requires([css('bootstrap-theme-3.3.5.min.css')]),virtual(true)]
  ).
:- endif.

:- if(debugging(js(bootstrap))).
  :- html_resource(
    js(bootstrap),
    [requires([js(jquery),js('bootstrap-3.3.5.js')]),ordered(true),virtual(true)]
  ).
:- else.
  :- html_resource(
    js(bootstrap),
    [requires([js(jquery),js('bootstrap-3.3.5.min.js')]),ordered(true),virtual(true)]
  ).
:- endif.
