/**  <module>  Maps display

    A  provider neutral maps component.

     Weblog
     Licensed under LGPL
*/

:- module(map,
	  [
	    geo_map//1,
	    average_geopoints/2
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(settings)).

:- use_module(library(info/maps/google/gmap)).
:- use_module(library(info/maps/leaflet/leafletmap)).


:- meta_predicate geo_map(1, ?, ?).

/**    geo_map(+Generator:closure)// is det

Geomap (map of Earth) component.

Generator is an arity n term that corresponds to an arity n+1
predicate.

geo_map//1 will repeatedly query Generator for information and build up
the map.  The final argument may be

  * provider(-Name)  one of leaflet or google. default google

  * id(-ID) The map div id and javascript variable name will be set to
  this. default lmap or gmap depending on provider. must be valid
  javascript identifier as atom.

  * zoom(Zoom) The zoom level. Provider specific how this maps to a
  viewport. Default 14

  * center(Lat, Long) center map view here. defaults to average of
  points

  * point(-Lat, -Long) A marker will be placed at this point

  * icon_for(+point(Lat, Long), -IconName) icon to use for this point.
  default is provider default icon.

  * tooltip_for(+point(Lat, Long), -ToolTipText)  contents of tooltip
  default is no tooltip

  * popup_for(-HTML, +point(Lat, Long)) termerized HTML to put in popup

  * style(-Style) only meaningful for leaflet, is cloudmade style
  number

  * maptype(-Type) only meaningful for google maps, is the constant for
  google.maps.MapTypeId (one of 'HYBRID', 'ROADMAP', 'SATELLITE', 'TERRAIN')


Defining icon types means binding an icon/3 for each type, then binding
all the properties

  * icon(-Name, -ImageSource, -ShadowSource) Defines an icon type name.
  The actual javascript names are common for all maps with same
  provider, so you should only define the icons for each map provider
  once.

Defining an icon requires that the following be defined for each icon
  type name:

  * * icon_size(+Name, X, Y) size of icon image

  * * shadow_size(+Name, X, Y) size of shadow image

  * * icon_anchor(+Name, X, Y) offset from UL of image to the point
  touching the spot on the map

  * * shadow_anchor(+Name, X, Y) offset
  from UL of shadow image to the point touching the spot on map

  * * popup_anchor(+Name, X, Y) offset from the point touching map to
  where the popup appears (so, eg, Y coord is often negative)

  @tbd add an example to docs

*/
geo_map(Generator) -->
	{
	     (	 call(Generator, provider(P))
	     ;
		 P = google
	     )
	},
	make_geo_map(P, Generator).

geo_map(_Generator) -->
	{
		throw(error(domain_error(list, 'provider'), context(geo_map//2,
				   'invalid provider')))
	},
	html([p('error - cannot make map')]).


make_geo_map(leaflet, Generator) -->
	lmap(Generator).
make_geo_map(google, Generator) -->
	gmap(Generator).

/**    average_geopoints(+Points:list, -MeanPoint:point) is det

     returns 0,0 for empty list
*/
average_geopoints([] , point(0.0, 0.0)) :- !.
average_geopoints(Coordinates, point(ALat, ALong)) :-
	sum_geopoints(Coordinates, 0, SumLat, 0, SumLong),
	length(Coordinates, Count),
	ALat is SumLat/Count,
	ALong is SumLong/Count.

sum_geopoints([], Lat, Lat, Long, Long).
sum_geopoints([point(Lat, Long)|T], Lat0, LatS, Long0, LongS) :-
	Lat1 is Lat0+Lat,
	Long1 is Long0+Long,
	sum_geopoints(T, Lat1, LatS, Long1, LongS).
