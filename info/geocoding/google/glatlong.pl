:- module(glatlong, [gaddr_latlong/6]).

/** <module> Lat/long from street address

    Calls Google api to get latlong info for street address.

*/

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).


:- dynamic(country_location/2).


/**  gaddr_latlong(+Name:codes,
		  -FormattedAddress:atom_or_codes,
		  -Type:atom,
		  -Bounds:box,
		  -Location:point,
		  -ViewPort:box) is det

		  box is a term of form box(NELat, NELong, SWLat, SWLong)
		  point is a term of form point(Lat, Long)

		  type is an atom - known responses include
		  * street_address
		  * locality
		  * establishment  (only seen for 'africa' so far)

*/
gaddr_latlong(Name,
	      FA,
	      TYPE,
	      box(BoundNELat, BoundNELong, BoundSWLat, BoundSWLong),
	      point(Loc_Lat, Loc_Long),
	      box(View_NELat, View_NELong, View_SWLat, View_SWLong)
	      ) :-
	(   is_list(Name) -> atom_codes(AName , Name) ; AName = Name),
	www_form_encode(AName , FEName),
	atom_concat('http://maps.googleapis.com/maps/api/geocode/json?address=',
		    FEName , PreReq),
	atom_concat(PreReq, '&sensor=false', Req),
	http_open(Req, Stream, []),
	json_read(Stream , Term),
	close(Stream),
	json([results=[json(Results)|_],status='OK']) = Term,
	(   member(formatted_address=FA, Results) ; FA=Name),
	(   member(types=[TYPE|_], Results) ; TYPE=unknown),
	member(geometry=json(GEO), Results),
	member(bounds=json([
                    northeast=json([lat=BoundNELat,lng=BoundNELong]),
                    southwest=json([lat=BoundSWLat,lng=BoundSWLong])]), GEO),
	member(location=json([lat=Loc_Lat, lng=Loc_Long]), GEO),
	member(viewport=json([
                    northeast=json([lat=View_NELat,lng=View_NELong]),
                    southwest=json([lat=View_SWLat,lng=View_SWLong])]), GEO).
