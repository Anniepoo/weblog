:- module(map_demo, []).
/** <module>  Demo handler for maps

*/
% basic dispatch
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(weblog(info/maps/map)).

:- http_handler(root(map), map_handler, [id(map)]).

map_handler(_Request) :-
	reply_html_page(
	    title('Map Demo'),
	    [
	     h1('Map demo page'),
	     h2('Google Maps'),
	     p('Weblog contributors'),
	    \map([provider(google([])), id(gmap)], [
		      point(37.482214,-122.176552),     % Annie
		      point(37.969368,23.732979),       % Acropolis
		      point(52.334434,4.863596),        % VNU
		      point(29.720576,-95.34296)        % Univ. of Houston
		       ]),
	     h2('Leaflet'),
	     p('Weblog contributors'),
	    \map([provider(leaflet([])), id(leafletmap)], [
		      point(37.482214,-122.176552),     % Annie
		      point(37.969368,23.732979),       % Acropolis
		      point(52.334434,4.863596),        % VNU
		      point(29.720576,-95.34296)        % Univ. of Houston
		       ])

	    ]).

