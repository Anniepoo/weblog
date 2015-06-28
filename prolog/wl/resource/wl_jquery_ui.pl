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

:- html_resource(
  css('jquery-ui'),
  [
    requires(['https://code.jquery.com/ui/1.11.4/themes/ui-lightness/jquery-ui.css']),
    virtual(true)
  ]
).

:- html_resource(
  js('jquery-ui'),
  [
    requires([js(jquery),css('jquery-ui'),'http://code.jquery.com/ui/1.11.4/jquery-ui.js']),
    virtual(true)
  ]
).
