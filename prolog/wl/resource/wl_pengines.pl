:- module(wl_pengines, []).

/** <module>

Resource definition for including Pengine JavaScript code in HTML.

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2015
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

user:file_search_path(js, library(wl/resource/js)).

:- http_handler(js(.), serve_files_in_directory(js), [prefix]).

:- html_resource(js(pengines), [requires([js('pengines.js')]),virtual(true)]).
