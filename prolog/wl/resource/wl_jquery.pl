:- module(wl_jquery, []).

/** <module> Weblog version of jQuery

Defines the HTML resources for including jQuery into a Web page.

@author Anne Ogborn
@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2015
*/

:- use_module(library(debug)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

user:file_search_path(css, library(wl/resource/css)).

:- http_handler(css(.), serve_files_in_directory(css), [prefix]).

:- if(debugging(js(jquery))).
  :- html_resource(
    js(jquery),
    [requires([js('jquery-2.1.4.js')]),virtual(true)]
  ).
:- else.
  :- html_resource(
    js(jquery),
    [requires([js('jquery-2.1.4.min.js')]),virtual(true)]
  ).
:- endif.
