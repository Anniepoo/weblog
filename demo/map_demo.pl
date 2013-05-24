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
'#gmap, #leafletmap {
    width: 80%;
    height: 400px;
       }
'),
	     h1('Map demo page'),
	     h2('Google Maps'),
	     p('Weblog contributors'),
	     \geo_map([provider(google([])), id(gmap)], gmap_info),
	     h2('Leaflet'),
	     p('VNU Campus'),
	    \geo_map_direct([provider(leaflet([])), id(leafletmap)], [
		      point(52.334287,4.862677)+
                      popup(div([p(b('Free University Amsterdam')),
	                     p('Home of "SWI-Prolog"')]))+open  % VNU
		       ])
	    ]).


:- html_meta gmap_info(-, html).

gmap_info(map, _).
gmap_info(point(37.482214,-122.176552), _).    % Annie
gmap_info(point(37.969368,23.732979), _).       % Acropolis
gmap_info(point(52.334434,4.863596), _).        % VNU
gmap_info(point(29.720576,-95.34296), _).       % Univ. of Houston

