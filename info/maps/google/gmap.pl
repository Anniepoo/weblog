/**  <module>  Google Maps display

    A (for now) rudimentary google maps component.

     This code provided to weblog by Cliopatria project
     Licensed under LGPL
*/

:- module(gmap,
	  [ gmap//1,			% +Coordinates
	    gmap//2                     % +Options, +Coordinates
	  ]).

:- use_module(library(http/html_write)).
:- ensure_loaded(weblog(resources/resources)).

:- include(weblog('keys/googlekey.pl')).

%%	gmap(+Coordinates)// is det.
%
%	HTML component that shows Google maps  with markers at the given
%	Coordinates. Coordinates is a list. Each  coordinate is a
%	term point(Lat,Long). This can be extended by defining
%	more gmap:coordinate/3 clauses
%
%	@bug For some reason this does not work if you set dialect to xhtml
gmap(Coordinates) -->
	gmap([], Coordinates).

:- predicate_options(gmap//2, 1, [
	id(text)
	       ]).

gmap(Options, Coordinates) -->
	{
	    option(id(ID), Options, map_canvas),
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
	show_map(Coordinates, ID).

gmap(_, _) -->
	html([p('Missing google key in weblog/keys/googlekey.pl.example')]).

show_map(Coordinates, ID) -->
	{ avg(Coordinates, point(CLat, CLong))
	},
	html(script(type('text/javascript'),
		    [ 'if (GBrowserIsCompatible()) {\n',
		      'var ~w = new GMap2(document.getElementById("~w"));\n'-[ID,ID],
		      '~w.setCenter(new GLatLng(~w,~w), 2);\n'-[ID, CLat, CLong],
		      \coords(ID, Coordinates),
		      '~w.setUIToDefault();\n'-[ID],
		     % '~w.addControl(new GSmallMapControl());\n'-[ID],
		      '}\n'
		    ])).

coords(_, []) --> [].
coords(ID, [C|T]) -->
	{ coordinate(C, Lat, Long) },
	html('~w.addOverlay(new GMarker(new GLatLng(~w,~w)));\n'-[ID,Lat,Long]),
	coords(ID, T).

avg([] , point(0.0, 0.0)) :- !.
avg(Coordinates, point(ALat, ALong)) :-
	sum_ll(Coordinates, 0, SumLat, 0, SumLong),
	length(Coordinates, Count),
	ALat is SumLat/Count,
	ALong is SumLong/Count.

sum_ll([], Lat, Lat, Long, Long).
sum_ll([C|T], Lat0, LatS, Long0, LongS) :-
	coordinate(C, Lat, Long),
	Lat1 is Lat0+Lat,
	Long1 is Long0+Long,
	sum_ll(T, Lat1, LatS, Long1, LongS).

:- multifile gmap:coordinate/3.

coordinate(point(Lat, Long), Lat, Long).
