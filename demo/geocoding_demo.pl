:- module(geocoding_demo, []).
/** <module>  Demo handler for geocoding and gps

*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).

:- use_module(weblog(info/maps/map)).
:- use_module(weblog(info/geocoding/google/glatlong)).

:- http_handler(root(geocoding), geocoding_handler, [id(geocoding)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(geocoding, 'Geocoding').

geocoding_handler(Request) :-
	http_parameters(Request, [
	   loc(Address, [default('VNU University, Amsterdam, Netherlands')])
				 ]),
	gaddr_latlong(Address, FA, Type, _Bnds, Loc, _View),
	reply_html_page(
	    title('GeoCoding Demo'),
	    [
	     style(
'#gmap, #leafletmap {
    width: 80%;
    height: 400px;
       }
'),
	     h1('Geocoding demo page'),
	     p('Enter a location (City, State works), page will reload with the location centered in map.'),
	     p('Geocoding also handles phrases like "coffee near Menlo Park, CA", this demo just centers the map on the first returned result'),
	     form(action=location_by_id(geocoding), [
		  input([name=loc, type=textarea], []),
	          input([type=submit, name=submit, value='Find It'])
						    ]),
	    \geo_map(geocoding_callback(Address, FA, Type, Loc)),
	    \gps_demo
	    ]).

geocoding_handler(_Request) :-
	reply_html_page(
	    title('Oops!'),
	    [p('sorry, couldn\'t geocode that')]).

geocoding_callback(_, _, _, _, id(leafletmap)).
geocoding_callback(_, _, _, _, provider(leaflet)).
geocoding_callback(_, _, _, _, zoom(14)).
geocoding_callback(_, _, _, _, style(97737)).  % the geohashing style
geocoding_callback(_, _, _, Pt, Pt).
geocoding_callback(Address, FA, Type, Pt, popup_for(
	    [
	       p(b(Address)),
	       p(['Canonical Name:', FA, ' (type: ', Type, ')'])
	    ], Pt)).


gps_demo -->
	html([
	    \html_requires(wl_gps),
	    h2('gps demo'),
	    div([class='', id=status], ['Data will be here'])
	]).
