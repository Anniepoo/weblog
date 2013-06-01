/**  <module>  Leaflet Maps display

    A (for now) rudimentary leaflet maps component.

     This code is part of the weblog project
     Licensed under LGPL
*/

:- module(leafletmap,
	  [ lmap//1			% +Generator
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).
:- use_module(weblog(support/html_comments)).
:- use_module(weblog(support/javascript)).
:- ensure_loaded(weblog(resources/resources)).

:- include(weblog('keys/cloudmadekey.pl')).


:- meta_predicate  lmap(1, ?, ?).

%%	lmap(+Generator)// is det.
%
%	HTML component that shows Open Street Map maps, using the
%	Leaflet library (leafletjs.com) with tiles provided by
%	Cloudmade (cloudmade.com).
%	Maps are generated from a closure. This is documented in
%	map:geo_map.
%
%	Do not call this directly, call it through geo_map and
%	pass provider(leaflet).
%
%
lmap(Generator) -->
	{
	    (	call(Generator, id(ID)) ; ID = lmap   )
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
	define_icons(Generator),
	show_map(Generator),!.
lmap(_) -->
	html(p('Missing cloudmade key in weblog/keys/cloudmade.pl. or other call error')).

define_icons(Generator) -->
	{
	    setof(Name, A^B^call(Generator, icon(Name, A, B)), Names),!
	},
	html(script(type('text/javascript'), [
	     \def_icons_helper(Generator, Names) ])).
% fallback if no icons defined
define_icons(_) --> [].

def_icons_helper(_, []) --> [].
def_icons_helper(Generator, [H|T]) -->
	{
	    call(Generator, icon(H, ImgSrc, MaskSrc)),
	    call(Generator, icon_size(H, IconSizeX, IconSizeY)),
	    call(Generator, shadow_size(H, ShadowSizeX, ShadowSizeY)),
	    call(Generator, icon_anchor(H, IconAnchorX, IconAnchorY)),
	    call(Generator, shadow_anchor(H, ShadowAnchorX, ShadowAnchorY)),
	    call(Generator, popup_anchor(H, PopupAnchorX, PopupAnchorY))
	},
	html([
	    'var ~wIcon = L.icon({~niconUrl: \'~w\',~n'-[H, ImgSrc],
	    '    shadowUrl: \'~w\',~n'-[MaskSrc],
	    '	 iconSize:   [~w, ~w],~n'-[IconSizeX, IconSizeY],
	    '	 shadowSize: [~w, ~w],~n'-[ShadowSizeX, ShadowSizeY],
	    '    iconAnchor: [~w, ~w],~n'-[IconAnchorX, IconAnchorY],
	    '	 shadowAnchor: [~w, ~w],~n'-[ShadowAnchorX, ShadowAnchorY],
	    '    popupAnchor: [~w, ~w]~n});~n'-[PopupAnchorX, PopupAnchorY]
	     ]),
	def_icons_helper(Generator, T).
def_icons_helper(Generator, [H|T]) -->
	html(\[' // ~w could not be generated (missing values?)~n'-[H]]),
	def_icons_helper(Generator, T).

show_map(Generator) -->
	{
	  (	call(Generator, id(ID)) ; ID = lmap   ),
	  (	call(Generator, zoom(Zoom)) ; Zoom = 14  ),
	  setof(point(X,Y), call(Generator, point(X,Y)), Coordinates),
	  setting(cloudmade_map_key, Key),
	  (     call(Generator, center(CLat, CLong)) ; avg(Coordinates, point(CLat, CLong)))
	},
	html(script(type('text/javascript'), [
'var ~w = L.map(\'~w\').setView([~w, ~w], ~w);\n'-[ID, ID, CLat, CLong, Zoom],
'L.tileLayer(\'http://{s}.tile.cloudmade.com/~w/997/256/{z}/{x}/{y}.png\', {\n'-[Key],
	'    maxZoom: 18\n',
'}).addTo(~w);\n'-[ID],
	     \coords(Generator, Coordinates)
		    ])).

coords(_, []) --> [].
coords(Generator, [point(Lat, Long)|T]) -->
	{
	  (	call(Generator, id(ID)) ; ID = lmap   ),
	 (   call(Generator, icon_for(point(Lat, Long), N)) ->
	     format(codes(IconName), ', {icon: ~wIcon}', [N])
	 ;
	     IconName = ""
	)
	},
	html(['L.marker([~w,~w]~s).addTo(~w)'-[Lat, Long, IconName, ID],
	     \decorations(Generator, point(Lat, Long)),
	     ';\n']),
	coords(Generator, T).

avg([] , point(0.0, 0.0)) :- !.
avg(Coordinates, point(ALat, ALong)) :-
	sum_ll(Coordinates, 0, SumLat, 0, SumLong),
	length(Coordinates, Count),
	ALat is SumLat/Count,
	ALong is SumLong/Count.

sum_ll([], Lat, Lat, Long, Long).
sum_ll([point(Lat, Long)|T], Lat0, LatS, Long0, LongS) :-
	Lat1 is Lat0+Lat,
	Long1 is Long0+Long,
	sum_ll(T, Lat1, LatS, Long1, LongS).



decorations(Generator, Pt) -->
	{
	   call(Generator, popup_for(HTML, Pt)),
	   javascript_friendly_html(HTML, JavascriptFriendlyHTML)
	},
	html('.bindPopup("'),
	html(\[JavascriptFriendlyHTML]),
	html('")'),!.
decorations(_, _) -->
	[].
