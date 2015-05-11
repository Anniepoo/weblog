:- module(glatlong, [gaddr_latlong/6]).

/** <module> Lat/long from street address

Calls Google API to get latitude/longitude information for a given
street address.

@author Anne Ogborn
@author Wouter Beek
@version 2013-2015
*/

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(uri)).

:- dynamic(country_location/2).



%! gaddr_latlong(
%!   +Name:atom,
%!  -FormattedAddress:atom_or_codes,
%!  -Type:atom,
%!  -Bounds:box,
%!  -Location:point,
%!  -ViewPort:box
%! ) is det.
% Box is a term of form `box(NELat, NELong, SWLat, SWLong)`.
% Point is a term of form `point(Lat, Long)`
%
% Type is an atom - known responses include
%   * street_address
%   * locality
%   * establishment (only seen for 'africa' so far)

gaddr_latlong(
  Address,
  FA,
  TYPE,
  box(BoundNELat, BoundNELong, BoundSWLat, BoundSWLong),
  point(Loc_Lat, Loc_Long),
  box(View_NELat, View_NELong, View_SWLat, View_SWLong)
):-
  uri_query_components(Query, [address=Address,sensor=false]),
  uri_components(
    Uri,
    uri_components(
      http,
      'maps.googleapis.com',
      '/maps/api/geocode/json',
      Query,
      _
    )
  ),
gtrace,
  setup_call_cleanup(
    http_open(Uri, Stream, []),
    json_read_dict(Stream, Dict),
    close(Stream)
  ),
  Dict.stats == 'OK',
  Results = Dict.results,
  (   FA = Results.get(formatted_address)
  ->  true
  ;   FA = Address
  ),
  (   [TYPE|_] = Results.get(types)
  ->  true
  ;   TYPE=unknown
  ),
  GEO = Results.geometry,
	member(bounds=json([
                    northeast=json([lat=BoundNELat,lng=BoundNELong]),
                    southwest=json([lat=BoundSWLat,lng=BoundSWLong])]), GEO),
	member(location=json([lat=Loc_Lat, lng=Loc_Long]), GEO),
	member(viewport=json([
                    northeast=json([lat=View_NELat,lng=View_NELong]),
                    southwest=json([lat=View_SWLat,lng=View_SWLong])]), GEO).
