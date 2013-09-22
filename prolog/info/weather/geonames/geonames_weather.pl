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

:- ensure_loaded(weblog(resources/resources)).

% TBD - this has a defect. settings are per-module, but
% we want all settings shared for geonames
%
:- setting(
  geonames_username,
  atom,
  'notarealgeonameskey',
  'Geonames key.  Get one at http://www.geonames.org/'
).

prolog:message(missing_key_file(File)) -->
  ['Key file ~w is missing.'-[File], nl].
:-
  % Print an error message if the keyfile is not present.
  (
    absolute_file_name(
      weblog('keys/geonameskey'),
      File,
      [access(read), file_errors(fail), file_type(prolog)]
    )
  ->
    load_settings(File)
  ;
    debug(weblog, 'Geonames key missing', [])
  ).

geo_weather_widget(_) -->
  {
    setting(geonames_username, notarealgeonameskey)
  },
  !,
  html([p('Missing geonames key in weblog/keys/geonameskey.pl')]).
geo_weather_widget(Generator) -->
	{
	    setting(geonames_username, UserName),
	    (	call(Generator, id(ID)) ; ID = weather),
	    generator_lat_long(Generator, Lat, Lng),
            uri_query_components(Qs, [lat(Lat), lng(Lng), username(UserName)]),
            format(atom(URI), 'http://api.geonames.org/astergdemJSON?~w', [Qs]),
            http_get(URI, _Reply, [])
        },
	html(div([id=ID, class=[weblog, 'weather-widget']],
		 p('someday a weather widget')
		)).

generator_lat_long(Generator, Lat, Lng) :-
	call(Generator, point(Lat, Lng)).


