:-module(geonames_weather, [geo_weather_widget//1]).
/** <module>  Geonames implementation of weather


this returns the astergdem height at that point
10 ?- http_get('http://api.geonames.org/astergdemJSON?lat=50.01&lng=10.2&username=anniepoo&style=full', Reply, []).
Reply = json([astergdem=192,lng=10.2,lat=50.01]).



*/
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(uri)).
:- use_module(library(wl/support/wl_setting)).



geo_weather_widget(Generator) -->
  {
    api_key(geonames, Key),
    (	call(Generator, id(ID)) ; ID = weather),
    generator_lat_long(Generator, Lat, Lng),
    uri_query_components(Qs, [lat(Lat),lng(Lng),username(Key)]),
    format(atom(URI), 'http://api.geonames.org/astergdemJSON?~w', [Qs]),
    http_get(URI, _Reply, [])
  },
	html(div([id=ID, class=[weblog, 'weather-widget']],
		 p('someday a weather widget')
		)).

generator_lat_long(Generator, Lat, Lng) :-
	call(Generator, point(Lat, Lng)).
