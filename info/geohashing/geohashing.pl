:- module(geohashing, [hash_point/3,
		       code_graticule/2,
		       code_graticule/3,
		       minesweeper/3]).
/** <module> Tools for geohashing
    Tools for the sport of geohashing.

    http://wiki.xkcd.com/geohashing/Main_Page

    Because there are two zeros in graticules (if
    the fractional part of the hash longitude is
    0.35, then the hash for +0 graticule is 0.35w
    and 0.35e for the -0 graticule.
    Thus
    So there's a bit of type here, with a graticule
    being a structure that handles all this.

*/

:- use_module(weblog(info/stock/crox/croxstock)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(dcg/basics)).


/**     code_graticule(?Code:codes, ?Grat:graticule) is semidet

        Attempts to convert codes to a graticule structure.
        Codes should look something like
	"-14,3"  (the 14 deg N latitude, 3 deg E longitude graticule)
	"52, 0"	 (Cambridge, UK graticule)
	"52, -0" (Northampton, UK graticule)

	@Code see above
	@Grat opaque graticule structure
*/
code_graticule(Code, graticule(Lat, Long)) :-
	var(Code),
	float(Lat),
	float(Long),
	Lat >= -90.0,
	Lat =< 90.0,
	Long >= -180.0,
	Long =< 180.0,
	codes_gnum(CLat, Lat),
	codes_gnum(CLong, Long),
	format(codes(Code), '~s, ~s', [CLat, CLong]).
code_graticule(Code, graticule(Lat, Long)) :-
	is_list(Code),
        phrase(grat_syntax(Lat, Long), Code),
	Lat >= -90.0,
	Lat =< 90.0,
	Long >= -180.0,
	Long =< 180.0 .

/**     code_graticule(?CodeLat:codes, ?CodeLong:codes, ?Grat:graticule) is semidet

        Attempts to convert a pair of codes to a graticule structure.
        Codes should look something like
	"-14"  (the 14 deg coord)
	"0"	 (the 0 coord)
	" -0" (the -0 coord)

	@Code see above
	@Grat opaque graticule structure
*/
code_graticule(CodeLat, CodeLong, graticule(Lat, Long)) :-
	var(CodeLat),
	codes_gnum(CodeLat, Lat),
	codes_gnum(CodeLong, Long),
	Lat >= -90.0,
	Lat =< 90.0,
	Long >= -180.0,
	Long =< 180.0 .
code_graticule(CodeLat, CodeLong, graticule(Lat, Long)) :-
	is_list(CodeLat),
	phrase(half_grat(Lat), CodeLat),
	phrase(half_grat(Long), CodeLong),
	Lat >= -90.0,
	Lat =< 90.0,
	Long >= -180.0,
	Long =< 180.0 .

/**    codes_gnum(-C:codes, +G:float) is det

   convert a graticule float value to codes
*/
codes_gnum("-0", -0.0) :- !.
codes_gnum(CG, G) :-
	I is truncate(G),
	format(codes(CG), '~d', [I]).

grat_syntax(Lat, Long) -->
	blanks, grat(Lat), frac_part, ",",
	blanks, grat(Long), frac_part, blanks.

half_grat(Coord) -->
	blanks, grat(Coord), blanks.

grat(-0.0) -->
	"-0",!.
grat(G) -->
	integer(I),
	{
	    G is 1.0 * I
	}.

frac_part --> [].
frac_part --> ".", digits(_).



/**   hash_point(+Date:term, +Lat:number, +Long:number, -Point:term) is det

      given a date and lat/long, returns point(Lat, Long)
 on 2008-05-27 the rules for computation of points E of 30W changed

   @tbd This is roaringly asking for memoization, or maybe crstock_stats
   is

*/
hash_point(YY - MM - DD, graticule(Lat, Long), point(PLat, PLong)) :-
	LLat is float_integer_part(Lat),
	LLong is float_integer_part(Long),
	(   YY < 1000 -> YYYY is YY + 2000 ; YYYY is YY),
	(   east_rule(YYYY - MM - DD, LLong) -> DDDD is DD - 1; DDDD is DD),
	normalize_geohash_date(YYYY - MM - DDDD, ZY - ZM - ZD),
	(   ZM < 10 -> MFill = '0' ; MFill = ''),
	(   ZD < 10 -> DFill = '0' ; DFill = ''),
	crstock_stats(djia, opening, ZY - ZM - ZD, Stock),
	format(atom(ToHash), '~d-~w~d-~w~d-~2f',
	       [ZY, MFill, ZM, DFill, ZD, Stock]),
	% The only available MD5 *DIGEST* implementation is from semweb
	rdf_atom_md5(ToHash, 1, HashedAtom),
	atom_codes(HashedAtom, HashedCodes),
	append(FirstHalf, LastHalf, HashedCodes),
	same_length(FirstHalf, LastHalf),!,
        hex_codes_frac_float(FirstHalf, 1.0, LatFrac),
	hex_codes_frac_float(LastHalf, 1.0, LongFrac),
	grat_geo(LLat , LatFrac, PLat),
	grat_geo(LLong, LongFrac, PLong).

/**   grat_geo(+GratNum, +Frac, -Geo) is det

	 get a real geocoord from a graticule number and
	 fraction
*/
grat_geo(-0.0, Frac, Geo) :-
	!,
	Geo is -1.0 * Frac.
grat_geo(Num, Frac, Geo) :-
	Geo is sign(Num) *(abs(Num) + Frac).



/**   minesweeper(+Date:term, +Grat:graticule, -Points:list) is det

      given a date and graticule, returns a list of nine
      point(Lat, Long) structures for the 9 nearest hash points

      note - at the poles returns a shorter list

*/

minesweeper(Date, Grat, Points) :-
	setof(Pt, a_ms_pt(Date, Grat, Pt), Points).

a_ms_pt(Date, Grat, Pt) :-
	member(OffLat, [-1, 0, 1]),
	member(OffLong, [-1, 0, 1]),
	hash_offset(Grat, OffLat, OffLong, NGrat),
	hash_point(Date, NGrat, Pt).

/**    hash_offset(+G:graticule, +OffLat:integer, +OffLong:integer, -NG:graticule) is semidet

       offset a graticule. fails if new graticule isn't on earth.
*/
hash_offset(graticule(Lat, Long), OffLat, OffLong, graticule(NLat, NNLong)) :-
	coord_offset(Lat, OffLat, NLat),
	NLat >= -90.0,
	NLat =< 90.0,
	coord_offset(Long, OffLong, NLong),
	(   NLong =< -180.0 ->
	    NNLong is NLong + 360.0 ;
	    NNLong is NLong
	),
	(   NLong >= 180.0 ->
	    NNLong is NLong - 360.0 ;
	    NNLong is NLong
	).

coord_offset(G, 0, G).
coord_offset(-0.0, 1, 0.0).
coord_offset(0.0, -1, -0.0).
coord_offset(G, N, NG) :- NG is N + G.

normalize_geohash_date(Y - M - D, YO - MO - DO) :-
	date_time_stamp(date(Y, M, D, 0,0,0,0,-,-), Stamp),
	stamp_date_time(Stamp, Dt, 0),
	date_time_value(date, Dt, date(YO, MO, DO)).


/**   east_rule(+Date:geohashdate, +IntLong:int) is semidet

	unifies if this location and date invokes the 'east of 30W' rule
*/
east_rule(YYYY - _ - _, IntLong) :-
	IntLong >= -30,
	YYYY > 2008.
east_rule(YYYY - MM - _, IntLong) :-
	IntLong >= -30,
	YYYY = 2008,
	MM > 5.
east_rule(YYYY - MM - DD, IntLong) :-
	IntLong >= -30,
	YYYY = 2008,
	MM = 5,
	DD >= 27.

/**   hex_codes_frac_float(+Codes:codes, +Place:float, -Value:float) is semidet

     converts a hex string to a fractional float value

     Codes is expected to be a list of ascii codes of hex digits,
     Place is the fractional place to the extreme left,
     Value is the floating pt equivilent


*/
hex_codes_frac_float([], _, 0.0).
hex_codes_frac_float([H|T], Place, Value) :-
	char_type(H, xdigit(D)),
	NewPlace is Place / 16.0,
	hex_codes_frac_float(T, NewPlace, RemVal),
	Value is NewPlace * D + RemVal.











