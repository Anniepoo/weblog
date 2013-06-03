/**  <module>  Google Maps display

    A (for now) rudimentary google maps component.

     This code provided to weblog by Cliopatria project
     Licensed under LGPL
*/

:- module(gmap,
	  [ gmap//1			% +Coordinates
	  ]).

:- use_module(library(http/html_write)).
:- ensure_loaded(weblog(resources/resources)).

:- include(weblog('keys/googlekey.pl')).

% needed for some coord calc stuff
:- use_module(weblog(info/maps/map)).

%%	gmap(+Generator)// is det.
%
%	HTML component that shows google maps
%	Maps are generated from a closure. This is documented in
%	map:geo_map.
%
%	Do not call this directly, call it through geo_map and
%	bind provider(google) (or do nothing, google is the default)
%
%

gmap(Generator) -->
	{
	    (	call(Generator, id(ID)) ; ID = gmap),
	    setting(google_map_key, Key),
	    setting(google_map_script, Script),!,
	    format(atom(Src),
		'~w&key=~w',
		[Script, Key])
	},
	html([  % \html_requires(googlemap),
	      \html_post(head, script([type('text/javascript'),
	                               src(Src)], []) ),
	      div([ id(ID)
		 ],
		 [])]),
	show_map(Generator).

gmap(_) -->
	html([p('Missing google key in weblog/keys/googlekey.pl.example or other problem')]).

show_map(Generator) -->
	{
	  (	call(Generator, id(ID)) ; ID = gmap   ),
	  (	call(Generator, zoom(Zoom)) ; Zoom = 14  ),
	  setof(point(X,Y), call(Generator, point(X,Y)), Coordinates),
	  (     call(Generator, center(CLat, CLong)) ; average_geopoints(Coordinates, point(CLat, CLong)))
	},
	html(script(type('text/javascript'),
		    [ 'if (GBrowserIsCompatible()) {\n',
		      'var ~w = new GMap2(document.getElementById("~w"));\n'-[ID,ID],
		      '~w.setCenter(new GLatLng(~w,~w), 2);\n'-[ID, CLat, CLong],
		      \coords(Generator, Coordinates),
		      '~w.setUIToDefault();\n'-[ID],
		     % '~w.addControl(new GSmallMapControl());\n'-[ID],
		      '}\n'
		    ])).

coords(_, []) --> [].
coords(Generator, [point(Lat, Long)|T]) -->
	{
	    (	call(Generator, id(ID)) ; ID = gmap   )
	},
	html('~w.addOverlay(new GMarker(new GLatLng(~w,~w)));\n'-[ID,Lat,Long]),
	coords(Generator, T).


