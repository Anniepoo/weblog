:- module(debug_demo, []).

/** <module> Debug demo

Loads the normal debug page as part of the **WebLog** demo.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(wl/page/debug_page)).

:- multifile(weblogdemo:label/2).
weblogdemo:label(debug_demo, 'debug tools').

:- http_handler(
  root(debug_demo),
  http_redirect(moved_temporary, location_by_id(debug_page)),
  [id(debug_demo)]
).
