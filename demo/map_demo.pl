:- module(map_demo, []).
/** <module>  Demo handler for maps

*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(weblog(info/maps/map)).

:- http_handler(root(map), map_handler, [id(map)]).

map_handler(_Request) :-
	reply_html_page(
	    [title('Map Demo'),
	     style(type('text/css'),
'#gmap, #lmap, #googleplex {
  width: 80%;
  height: 400px;
}')
	    ],
	    [
	     h1('Map demo page'),
	     h2('Really simple map'),
	     \geo_map(simple),
	     h2('Google Maps'),
	     p('Weblog contributors'),
	     \geo_map(gmap_info),
	     h2('Leaflet'),
	     p('VNU Campus'),
	    \geo_map(lmap_info)
	    ]).

simple(point(37.482214,-122.176552)).    % Annie

gmap_info(provider(google)).
gmap_info(id(googleplex)).
gmap_info(zoom(2)).
gmap_info(point(37.482214,-122.176552)).    % Annie
gmap_info(point(37.969368,23.732979)).       % Thanos - Acropolis
gmap_info(point(52.334434,4.863596)).        % VNU
gmap_info(point(29.720576,-95.34296)).       % Univ. of Houston
gmap_info(point(52.364767, 4.934787)).       % Wouter Beek

gmap_info(tooltip_for(point(37.482214,-122.176552),'Annie')).
gmap_info(tooltip_for(point(37.969368,23.732979), 'Thanos')).
gmap_info(tooltip_for(point(52.334434,4.863596), 'VNU')).
gmap_info(tooltip_for(point(29.720576,-95.34296), 'Univ. of Houston')).
gmap_info(tooltip_for(point(52.364767, 4.934787), 'Wouter Beek')).
gmap_info(popup_for([p(b('VNU - Home of SWI-Prolog!'))], point(52.334434,4.863596))).

lmap_info(provider(leaflet)).
lmap_info(style(2402)).   % cloudmade 'clean'
lmap_info(zoom(16)).
lmap_info(point(52.334434,4.863596)).        % VNU
lmap_info(icon_for(_, swiplpin)).
lmap_info(icon(swiplpin, '/icons/swiplpin.png', '/icons/swiplpinshadow.png')).
lmap_info(icon_size(swiplpin, 48, 61)).
lmap_info(shadow_size(swiplpin, 81, 61)).
lmap_info(icon_anchor(swiplpin, 24, 60)).
lmap_info(shadow_anchor(swiplpin, 24, 60)).
lmap_info(popup_anchor(swiplpin, 0, -55)).
lmap_info(popup_for([p(b('VNU - Home of SWI-Prolog!'))], _)).

