:- module(wl_jquery_ui, []).

/** <module> Weblog version of jQueryUI

Defines the HTML resources for including jQueryUI into a Web page.

@author Anne Ogborn
@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2015
*/

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

:- if(debugging(css('jquery-css'))).
  :- html_resource(
    css('jquery-ui'),
    [requires([css('jquery-ui-1.11.4.css')]), virtual(true)]
  ).
:- else.
  :- html_resource(
    css('jquery-ui'),
    [requires([css('jquery-ui-1.11.4-min.css')]), virtual(true)]
  ).
:- endif.

:- if(debugging(js('jquery-ui'))).
  :- html_resource(
    js('jquery-ui'),
    [ordered(true),requires([js(jquery),js('jquery-ui-1.11.4.js')]),virtual(true)]
  ).
:- else.
:- html_resource(
  js('jquery-ui'),
  [ordered(true),requires([js(jquery),js('jquery-ui-1.11.4-min.js')]),virtual(true)]
).
:- endif.
