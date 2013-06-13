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

/**    gmap(+Generator:closure)// is det

Geomap (map of Earth) component using Google Maps

Do not call this directly, call it through geo_map and
bind provider(google) (or do nothing, google is the default)

Generator is an arity n term that corresponds to an arity n+1
predicate.

gmap//1 will repeatedly query Generator for information and build up
the map.  The final argument may be

  * id(-ID) The map div id and javascript variable name will be set to
  this. default lmap or gmap depending on provider. must be valid
  javascript identifier as atom.

  * zoom(Zoom) The zoom level. Provider specific how this maps to a
  viewport. Default 14

  * center(Lat, Long) center map view here. defaults to average of
  points

  * point(-Lat, -Long) A marker will be placed at this point

  * icon_for(+point(Lat, Long), -IconName) icon to use for this point.
  default is provider default icon

  * popup_for(-HTML, +point(Lat, Long))termerized HTML to put in popup

  * style(-Style) only meaningful for leaflet, is cloudmade style
  number

Defining icon types means binding an icon/3 for each type, then binding
all the properties

  * icon(-Name, -ImageSource, -ShadowSource) Defines an icon type name.

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


