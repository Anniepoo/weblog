:- module(google_map_demo, []).
/** <module>  Demo handler for google maps

*/
% basic dispatch
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(weblog(info/maps/google/gmap)).

:- http_handler(root(googlemap), google_map_handler, [id(googlemap)]).

google_map_handler(_Request) :-
	reply_html_page(
	    title('Google Map Demo'),
	    [
	    h1('Google Map demo page'),
	     p('Weblog contributors'),
	    \gmap([
		      point(37.482214,-122.176552),     % Annie
		      point(37.969368,23.732979),       % Acropolis
		      point(52.334434,4.863596),        % VNU
		      point(29.720576,-95.34296)        % Univ. of Houston
		       ])
	    ]).

