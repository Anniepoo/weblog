:- module(debug_demo, []).
/** <module>  The debug demo just redirects to the normal debug page

     Though that may change some day.

*/

:- use_module(library(http/http_dispatch)).
:- use_module(weblog(debug_page/debug_page)).

:- multifile(weblogdemo:label/2).
weblogdemo:label(debug_demo, 'Debugging Tools').

:- http_handler(root(debugdemo) ,
	http_redirect(moved_temporary,
		      location_by_id(debug_page)),
	[id(debug_demo)]).

