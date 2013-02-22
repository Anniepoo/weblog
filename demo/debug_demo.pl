:- module(debug_demo, []).
/** <module>  The debug demo just redirects to the normal debug page

     Though that may change some day.

*/

:- use_module(library(http/http_dispatch)).
:- ensure_loaded(weblog(debug_page/debug_page)).

:- http_handler(root(debugdemo) ,
	http_redirect(moved_temporary,
		      location_by_id(debug_page)),
	[id(debug_demo)]).



