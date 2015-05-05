:- module(map_demo, []).

/** <module> Map demo

Generates an HTML demo page for maps.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(wl/info/map/map)).

:- http_handler(root(map), map_demo, [id(map)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(map, 'maps').

map_demo(_) :-
	reply_html_page(
    wl_demo,
	  [
      title('Map Demo'),
	    style(type('text/css'),
'#google_map, #leaflet_map, #googleplex {
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
	    p('VU University Amsterdam (VUA) campus'),
	    \geo_map(lmap_info)
	  ]
  ).

simple(point(37.482214,-122.176552)).   % Annie

gmap_info(provider(google)).
gmap_info(id(googleplex)).
gmap_info(zoom(2)).
gmap_info(point(37.482214,-122.176552)).   % Annie
gmap_info(point(37.969368,23.732979)).     % Thanos - Acropolis
gmap_info(point(52.334434,4.863596)).      % VU University Amsterdam
gmap_info(point(29.720576,-95.34296)).     % University of Houston
gmap_info(point(52.364767, 4.934787)).     % Wouter Beek

gmap_info(tooltip_for(point(37.482214,-122.176552),'Annie')).
gmap_info(tooltip_for(point(37.969368,23.732979), 'Thanos')).
gmap_info(tooltip_for(point(52.334434,4.863596), 'VNU')).
gmap_info(tooltip_for(point(29.720576,-95.34296), 'Univ. of Houston')).
gmap_info(tooltip_for(point(52.364767, 4.934787), 'Wouter Beek')).
gmap_info(
  popup_for([p(b('VUA - Home of SWI-Prolog!'))], point(52.334434,4.863596))
).

lmap_info(provider(leaflet)).
lmap_info(style(2402)).   % cloudmade 'clean'
lmap_info(zoom(16)).
lmap_info(point(52.334434,4.863596)).        % VUA
lmap_info(icon_for(_, swiplpin)).
lmap_info(icon(swiplpin, '/icons/swiplpin.png', '/icons/swiplpinshadow.png')).
lmap_info(icon_size(swiplpin, 48, 61)).
lmap_info(shadow_size(swiplpin, 81, 61)).
lmap_info(icon_anchor(swiplpin, 24, 60)).
lmap_info(shadow_anchor(swiplpin, 24, 60)).
lmap_info(popup_anchor(swiplpin, 0, -55)).
lmap_info(
  popup_for([p(b('VU University Amsterdam - Home of SWI-Prolog!'))], _)
).
