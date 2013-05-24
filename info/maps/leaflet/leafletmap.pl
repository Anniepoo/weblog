/**  <module>  Leaflet Maps display

    A (for now) rudimentary leaflet maps component.

     This code is part of the weblog project
     Licensed under LGPL
*/

:- module(leafletmap,
	  [ lmap//1,			% +Coordinates
	    lmap//2                     % +Options, +Coordinates
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(weblog(support/html_comments)).
:- use_module(weblog(support/javascript)).
:- ensure_loaded(weblog(resources/resources)).

:- include(weblog('keys/cloudmadekey.pl')).

%%	lmap(+Coordinates)// is det.
%
%	HTML component that shows Open Street Map maps, using the
%	Leaflet library (leafletjs.com) with tiles provided by
%	Cloudmade (cloudmade.com).
%       Maps have markers at the given
%	Coordinates. Coordinates is a list. Each coordinate is a term
%	point(Lat,Long). This can be extended by defining more
%	gmap:coordinate/3 clauses
%
%  @tbd abstract as much of the shared code betwen here and
%  google maps as possible
%
lmap(Coordinates) -->
	lmap([], Coordinates).

:- predicate_options(lmap//2, 1, [
	id(text)
	       ]).

lmap(Options, Coordinates) -->
	{
	    option(id(ID), Options, map),
	    setting(cloudmade_map_key, Key),!
	},
	html([
	      \html_requires(leaflet),
	      \html_post(head,
		\if_ie('lte IE 8',
                  link([ rel(stylesheet),
                    href('http://cdn.leafletjs.com/leaflet-0.5/leaflet.ie.css')
                  ]))),
	      div([ id(ID)
		 ],
		 [])]),
	show_map(Coordinates, ID, Key).
lmap(_, _) -->
	html(p('Missing cloudmade key in weblog/keys/cloudmade.pl.')).

show_map(Coordinates, ID, Key) -->
	{ avg(Coordinates, point(CLat, CLong))
	},
	html(script(type('text/javascript'), [
'var ~w = L.map(\'~w\').setView([~w, ~w], 14);\n'-[ID, ID, CLat, CLong],
'L.tileLayer(\'http://{s}.tile.cloudmade.com/~w/997/256/{z}/{x}/{y}.png\', {\n'-[Key],
	'    maxZoom: 18\n',
'}).addTo(~w);\n'-[ID],
	     \coords(ID, Coordinates)
		    ])).

coords(_, []) --> [].
coords(ID, [C|T]) -->
	{
	  coordinate(C, Lat, Long)
	},
	html(['L.marker([~w,~w]).addTo(~w)'-[Lat, Long, ID],
	     \decorations(ID, C),
	     ';\n']),
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
coordinate('+'(A,_), Lat, Long) :-
	coordinate(A, Lat, Long).

decorations(ID, '+'(Left, Right)) -->
	!,
	html([
	   \decorations(ID, Left),
	   \a_decoration(ID, Right)
	     ]).

decorations(_, point(_, _)) -->
	[].
decorations(ID, X) -->
	a_decoration(ID, X).

a_decoration(_, popup(HTML)) -->
	{
	   javascript_friendly_html(HTML, JavascriptFriendlyHTML)
	},
	html('.bindPopup("'),
	html(\[JavascriptFriendlyHTML]),
	html('")').

a_decoration(_, open) -->
	html('.openPopup()').
