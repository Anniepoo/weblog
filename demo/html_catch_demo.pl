:- module(html_catch_demo, []).

/** <module> HTML catch demo

Demo page showing the HTML version of the Prolog exception catcher.

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2015
*/

:- use_module(library(debug)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(wl/support/html_meta)).

:- http_handler(root(html_catch), html_catch_demo, [id(html_catch)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(html_catch, 'HTML catch').

html_catch_demo(Request):-
  reply_html_page(
    wl_demo,
    title('HTML catch'),
    html(\html_catch(html(\contents(Request))))
  ).

contents(Contents) -->
  {length(Length, Contents)},
  html(Length).
