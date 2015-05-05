:- module(
  google_map,
  [
    google_map//1 % +Coordinates
  ]
).

/**  <module>  Google Maps display

A (for now) rudimentary google maps component.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/html_write)).
:- use_module(library(uri)).
:- use_module(library(wl/info/map/map)).
:- use_module(library(wl/support/wl_setting)).

gmap_scheme(_NoScheme).
gmap_authority('maps.googleapis.com').
gmap_path('/maps/api/js').
gmap_fragment(_NoFragment).



%! google_map(:Generator)// is det
% Geomap (map of Earth) component using Google Maps
%
% Do not call this directly, call it through geo_map and
% bind provider(google) (or do nothing, google is the default)
%
% Generator is an arity n term that corresponds to an arity n+1
% predicate.
%
% google_map//1 will repeatedly query Generator for information and build up
% the map.
%
% ### Arguments for Generator
%
% Coordinate is a compound term of the form:
% ```prolog
% point(Lattitude:float,Longitude:float)
% ```
%
% The additional argument to Generator must be of the following forms:
%
%  * id(-ID)
%  The map div id and javascript variable name will be set to this.
%  Default `leaflet_map` or `google_map` depending on provider.
%  Must be valid javascript identifier as an atom.
%
%  * zoom(-Zoom:nonneg)
%  The zoom level.
%  Provider specific how this maps to a viewport.
%  Default is 14.
%
%  * center(-Lattitude:float, -Longitude:float)
%  Center the map view here.
%  Defaults to the average of the supplied points.
%
%  * point(-Lattitude:float, -Longitude:float)
%  A marker will be placed at this point
%
%  * icon_for(+Point:compound, -IconName:atom)
%  The icon that is used for the given coordinate.
%  Default is the provider's default icon.
%
%  * popup_for(-HTML, +Point:compound)
%  Termerized HTML to put in the popup for the given coordinate.
%
%  * tooltip_for(+Point:compound, -ToolTipText:atom)
%  Sets the contents of the tooltip for the given coordinate.
%
%  * maptype(-Type:oneof(['HYBRID','ROADMAP','SATELLITE','TERRAIN']))
%  Only meaningful for Google Maps
%  The constant for `google.maps.MapTypeId`.
%  Either `HYBRID`, `ROADMAP`, `SATELLITE`, or `TERRAIN`.
%
% Defining icon types means binding an icon/3 for each type, then binding
% all the properties
%
%  * icon(-Name, -ImageSource, -ShadowSource) Defines an icon type name.
%
% Defining an icon requires that the following be defined for each icon
% type name:
%
%  * * icon_size(+Name, X, Y) size of icon image
%
%  * * shadow_size(+Name, X, Y) size of shadow image
%
%  * * icon_anchor(+Name, X, Y) offset from UL of image to the point
%  touching the spot on the map
%
%  * * shadow_anchor(+Name, X, Y) offset
%  from UL of shadow image to the point touching the spot on map
%
%  * * popup_anchor(+Name, X, Y) offset from the point touching map to
%  where the popup appears (so, eg, Y coord is often negative)
%
% @tbd Add examples to the documentation.

google_map(Generator) -->
  {
    (call(Generator, id(ID)) ; ID = google_map),
    api_key(google_map, Key),
    Key \= notarealgooglekey, !,
    gmap_scheme(Scheme),
    gmap_authority(Authority),
    gmap_path(Path),
    uri_query_components(Search, [key=Key,sensor=false,v=3]),
    uri_components(
      URI,
      uri_components(Scheme,Authority,Path,Search,_)
    )
  },
  html([
    \html_post(head, script([src(URI), type('text/javascript')], [])),
    \html_post(head, [\show_map(Generator)]),
    div(id(ID), [])
  ]).
google_map(_) -->
	html([p('Google Maps failed')]).

show_map(Generator) -->
	{
	  (	call(Generator, id(ID)) ; ID = google_map   ),
	  (	call(Generator, zoom(Zoom)) ; Zoom = 14  ),
	  (     call(Generator, maptype(MT)),
		member(MT, ['HYBRID', 'ROADMAP', 'SATELLITE', 'TERRAIN'])
	  ;
		MT = 'TERRAIN' ),
	 % setof fails if the goal always does
	  (   setof(point(X,Y), call(Generator, point(X,Y)), Coordinates) ;
	      Coordinates = []),
	  (
	        call(Generator, center(CLat, CLong))
	  ;     average_geopoints(Coordinates, point(CLat, CLong))
	  )
	},
	define_icons(Generator),
	html(script(type('text/javascript'), [
\['	var ~w;
	function initialize() {
        var mapOptions = {
          center: new google.maps.LatLng(~w, ~w),
          zoom: ~w,~n'-[ID,CLat, CLong, Zoom],
'          mapTypeId: google.maps.MapTypeId.~w,
	  mapTypeControl: false
        };
'-[MT],
'        ~w = new google.maps.Map(document.getElementById("~w"),
            mapOptions);~n'-[ID, ID],
         \coords(Generator, Coordinates),
'~n      }
      google.maps.event.addDomListener(window, \'load\', initialize);~n'-[ID,ID]]])).

coords(_, []) --> [].
coords(Generator, [point(Lat, Long)|T]) -->
	{
	    (	call(Generator, id(ID)) ; ID = google_map   ),
	    (
	        call(Generator, icon_for(point(Lat, Long), N)),
		format(atom(IconAtom), ',   icon: ~wIcon,~n', [N])
	    ;
	        IconAtom = ''
	    ),
            (	call(Generator, tooltip_for(point(Lat, Long), ToolTip)),
		format(atom(ToolTipText), ',   title: "~w"\n',[ToolTip])
	    ;
	        ToolTipText = ''
	    )
	},
	html('(new google.maps.Marker({
    position: new google.maps.LatLng(~w, ~w)
~w~w
})).setMap(~w);~n'-[Lat, Long, IconAtom, ToolTipText, ID]),
	coords(Generator, T).

define_icons(Generator) -->
	{setof(Name, A^B^call(Generator, icon(Name, A, B)), Names)}, !,
	html(script(type('text/javascript'), [
	     \def_icons_helper(Generator, Names) ])).
% fallback if no icons defined
define_icons(_) --> [].

def_icons_helper(_, []) --> [].
def_icons_helper(Generator, [H|T]) -->
	{
	    call(Generator, icon(H, ImgSrc, _)),
	    call(Generator, icon_size(H, IconSizeX, IconSizeY)),
	    call(Generator, icon_anchor(H, IconAnchorX, IconAnchorY))
	},
	html([
	    'var ~wIcon = {
    url: \'~w\',~n'-[H, ImgSrc],
	    '	 size:	 new google.maps.Size(~w, ~w),~n'-[IconSizeX, IconSizeY],
	    '    origin: new google.maps.Point(0,0),
   iconAnchor: new google.maps.Point(~w, ~w)
};~n'-[IconAnchorX, IconAnchorY]
	     ]),
	def_icons_helper(Generator, T).
def_icons_helper(Generator, [H|T]) -->
	html(\[' // ~w could not be generated (missing values?)~n'-[H]]),
	def_icons_helper(Generator, T).
