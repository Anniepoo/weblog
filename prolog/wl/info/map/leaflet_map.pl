:- module(
  leaflet_map,
  [
    leaflet_map//1 % :Generator
  ]
).

/**  <module>  Leaflet Maps display

A (for now) rudimentary leaflet maps component.

Leaflet Maps requires a tile provider.
**WebLog** supports the Cloudmade title provider.

---

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(wl/info/html/html_comment)).
:- use_module(library(wl/info/map/map)).
:- use_module(library(wl/support/wl_setting)).
:- use_module(library(wl/support/wl_util)).

:- meta_predicate leaflet_map(1,?,?).

:- html_resource(
  leaflet,
  [
    requires([
      'http://cdn.leafletjs.com/leaflet-0.5/leaflet.css',
      'http://cdn.leafletjs.com/leaflet-0.5/leaflet.js'
    ]),
    virtual(true)
  ]
).



%! leaflet_map(:Generator)// is det.
% Generates an HTML element that shows Open Street Map maps.
%
%	Maps are generated from the closure Generator.
% The operation of the closure is documented in ../geo_map.pl
%
%	Do not call this directly but through ../geo_map.pl and
%	pass closure provider(leaflet).
%
% @see This uses the Leaflet library http://leafletjs.com
% @see Tiles are provided by CloudMade http://cloudmade.com

leaflet_map(Generator) -->
	{(call(Generator, id(ID)) ; ID = leaflet_map)},
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
leaflet_map(_) -->
	html(p('Leaflet failed')).

define_icons(Generator) -->
	{setof(Name, A^B^call(Generator, icon(Name, A, B)), Names)}, !,
	html(script(type('text/javascript'), \def_icons_helper(Generator, Names))).
% Fallback clause in case no icons are defined.
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
	    'var ~wLeafIcon = L.icon({~niconUrl: \'~w\',~n'-[H, ImgSrc],
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
	  (	call(Generator, id(ID)) ; ID = leaflet_map   ),
	  (	call(Generator, zoom(Zoom)) ; Zoom = 14  ),
	    % setof fails if the goal never succeeds
	  (   setof(point(X,Y), call(Generator, point(X,Y)), Coordinates) ;
	      Coordinates = []),
	  api_key(cloudmade, Key),
           Key \= notarealcloudmadekey,
	  (     call(Generator, center(CLat, CLong)) ; average_geopoints(Coordinates, point(CLat, CLong))),
	  (     call(Generator, style(Style)) ; Style = 997)
	},
	html(script(type('text/javascript'), [
'var ~w = L.map(\'~w\').setView([~w, ~w], ~w);\n'-[ID, ID, CLat, CLong, Zoom],
'L.tileLayer(\'http://{s}.tile.cloudmade.com/~w/~w/256/{z}/{x}/{y}.png\', {\n'-[Key, Style],
	'    maxZoom: 18,
	     minZoom: 2',
'}).addTo(~w);\nvar allmarkers = L.layerGroup().addTo(~w);\n'-[ID, ID],
	     \coords(Generator, Coordinates)
		    ])).

% needed because var_branches doesnt suppress the error
:- style_check(-singleton).
coords(_, []) --> [].
coords(Generator, [point(Lat, Long)|T]) -->
	{
	 (   call(Generator, tooltip_for(point(Lat, Long), ToolTip)) ; ToolTip = '' ),
	 (   call(Generator, id(ID)) ; ID = leaflet_map   ),
	 (   call(Generator, icon_for(point(Lat, Long), N)) ->
	     format(codes(IconName), ', {icon: ~wLeafIcon, title: \'~w\'}', [N, ToolTip])
	 ;
	     IconName = ""
	),
    % did this to avoid having entities made
        format(atom(MarkerCode), 'L.marker([~w,~w]~s).addTo(allmarkers)',[Lat, Long, IconName])
	},
	html([\[MarkerCode],
	     \decorations(Generator, point(Lat, Long)),
	     ';\n']),
	coords(Generator, T).
:- style_check(+singleton).

decorations(Generator, Pt) -->
	{
	   call(Generator, popup_for(HTML, Pt)),
	   js_friendly_html(HTML, JavascriptFriendlyHTML)
	},
	html('.bindPopup("'),
	html(\[JavascriptFriendlyHTML]),
	html('")'), !.
decorations(_, _) --> html([]).
