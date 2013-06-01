:- module(map_demo, []).
/** <module>  Demo handler for maps

*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(weblog(info/maps/map)).

:- http_handler(root(map), map_handler, [id(map)]).

map_handler(_Request) :-
	reply_html_page(
	    title('Map Demo'),
	    [
	     style(
'#gmap, #lmap {
    width: 80%;
    height: 400px;
       }
'),
	     h1('Map demo page'),
	     h2('Google Maps'),
	     p('Weblog contributors'),
	     \geo_map(gmap_info),
	     h2('Leaflet'),
	     p('VNU Campus'),
	    \geo_map(lmap_info)
	    ]).


gmap_info(provider(google)).
gmap_info(zoom(2)).
gmap_info(point(37.482214,-122.176552)).    % Annie
gmap_info(point(37.969368,23.732979)).       % Thanos - Acropolis
gmap_info(point(52.334434,4.863596)).        % VNU
gmap_info(point(29.720576,-95.34296)).       % Univ. of Houston

lmap_info(provider(leaflet)).
lmap_info(zoom(16)).
lmap_info(point(52.334434,4.863596)).        % VNU

