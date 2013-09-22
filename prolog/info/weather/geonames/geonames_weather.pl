:-module(geonames_weather, [geo_weather_widget//1]).
/** <module>  Geonames implementation of weather


10 ?- http_get('http://api.geonames.org/astergdemJSON?lat=50.01&lng=10.2&username=anniepoo&style=full', Reply, []).
Reply = json([astergdem=192,lng=10.2,lat=50.01]).

*/
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(uri)).

:- ensure_loaded(weblog(resources/resources)).

:- setting(
  geonames_username,
  atom,
  'iamnotavalidkeypasteoneinhere',
  'Geonames key.  Get one at http://www.geonames.org/'
).

prolog:message(missing_key_file(File)) -->
  ['Key file ~w is missing.'-[File], nl].
:-
  % Print an error message if the keyfile is not present.
  (
    absolute_file_name(
      weblog('keys/geonamekey'),
      File,
      [access(read), file_errors(fail), file_type(prolog)]
    )
  ->
    load_settings(File)
  ;
    print_message(warning, missing_key_file('geonamekey.pl'))
  ).

geo_weather_widget(Generator) -->
	{
	    setting(geonames_username, UserName),
	    (	call(Generator, id(ID)) ; ID = weather),
	    generator_lat_long(Generator, Lat, Lng),
            uri_query_components(Qs, [lat(Lat), lng(Lng), username(UserName)]),
            format(atom(URI), 'http://api.geonames.org/astergdemJSON?~w', [Qs]),
            http_get(URI, Reply, [])
        },
	html(div([id=ID, class=[weblog, 'weather-widget']],
		 p('someday a weather widget')
		)).



generator_lat_long(Generator, Lat, Lng) :-
	call(Generator, point(Lat, Lng)).


