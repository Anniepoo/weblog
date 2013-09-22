/**  <module>  Google Maps display

    A (for now) rudimentary google maps component.

     This code provided to weblog by Cliopatria project
     Licensed under LGPL
*/

:- module(gmap,
	  [ gmap//1			% +Coordinates
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(uri)).
:- ensure_loaded(weblog(resources/resources)).

% this makes sure there's always a setting
% weblog users - do NOT change this. Copy keys/googlekey.pl.example
% to keys/googlekey.pl and edit
:-   setting(
       google_map_key,
       atom,
       notarealgooglekey,   % don't change this here
       'Google map key.  "abcdefg" works for localhost (didn\'t for me -AO)'
).


gmap_scheme(_NoScheme).
gmap_authority('maps.googleapis.com').
gmap_path('/maps/api/js').
gmap_fragment(_NoFragment).

prolog:message(missing_key_file(File)) -->
  ['Key file ~w is missing'-[File], nl].
:-
  % Print an error message if the keyfile is not present.
  (
    absolute_file_name(
      weblog('keys/googlekey'),
      File,
      [access(read), file_errors(fail), file_type(prolog)]
    )
  ->
    load_settings(File, [undefined(error)])
  ;
% AO - 9/21/13 making this less in your face
%
% print_message(warning, missing_key_file('googlekey.pl'))
  debug(weblog, 'Google map key file missing (keys/googlekey.pl)', [])
  ).

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

  * tooltip_for(+point(Lat, Long), -ToolTipText)  contents of tooltip

  * maptype(-Type) only meaningful for google maps, is the constant for
  google.maps.MapTypeId (eg, 'HYBRID')

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
gmap(_) -->
  {
    setting(google_map_key, notarealgooglekey)
  },
  !,
  html([p('Missing google key in weblog/keys/googlekey.pl')]).
gmap(Generator) -->
  {
    (call(Generator, id(ID)) ; ID = gmap),
    setting(google_map_key, Key),
    Key \= notarealgooglekey,
    !,
    gmap_scheme(Scheme),
    gmap_authority(Authority),
    gmap_path(Path),
    uri_query_components(Search, [key=Key, sensor=false, v=3]),
    uri_components(
      URI,
      uri_components(Scheme, Authority, Path, Search, _Fragment)
    )
  },
  html([
    \html_post(head, script([src(URI), type('text/javascript')], [])),
    \html_post(head, [\show_map(Generator)]),
    div(id(ID), [])
  ]).

gmap(_) -->
	html([p('gmap failed')]).

show_map(Generator) -->
	{
	  (	call(Generator, id(ID)) ; ID = gmap   ),
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
	    (	call(Generator, id(ID)) ; ID = gmap   ),
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


