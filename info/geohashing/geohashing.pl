:- module(geohashing, [hash_point/3,
		       code_graticule/2,
		       code_graticule/3,
		       minesweeper/3,
		      globalhash/2]).
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


:- dynamic a_hash_point/3.

/**   hash_point(+Date:term, +Lat:number, +Long:number, -Point:term) is det

      given a date and lat/long, returns point(Lat, Long)
 on 2008-05-27 the rules for computation of points E of 30W changed

*/
hash_point(Date, Grat, Point) :-
	a_hash_point(Date, Grat, Point),!.

hash_point(Date, Grat, Pt) :-
	hash_atom( Date, Grat, HashAtom),
	point_from_atom(HashAtom, Grat, Pt),
	asserta(a_hash_point(Date, Grat, Pt)).

hash_atom( YY - MM - DD, graticule(_, Long), ToHash) :-
	LLong is float_integer_part(Long),
	(   YY < 1000 -> YYYY is YY + 2000 ; YYYY is YY),
	(   east_rule(YYYY - MM - DD, LLong) -> DDDD is DD - 1; DDDD is DD),
	normalize_geohash_date(YYYY - MM - DD, ZY - ZM - ZD),
	(   ZM < 10 -> MFill = '0' ; MFill = ''),
	(   ZD < 10 -> DFill = '0' ; DFill = ''),
	normalize_geohash_date(YYYY - MM - DDDD, StockY - StockM - StockD),
	crstock_stats(djia, opening, StockY - StockM - StockD, Stock),
	format(atom(ToHash), '~d-~w~d-~w~d-~2f',
	       [ZY, MFill, ZM, DFill, ZD, Stock]).

point_from_atom(ToHash, graticule(Lat, Long), point(PLat, PLong)) :-
	LLat is float_integer_part(Lat),
	LLong is float_integer_part(Long),
	% The only available MD5 *DIGEST* implementation is from semweb
	rdf_atom_md5(ToHash, 1, HashedAtom),
	atom_codes(HashedAtom, HashedCodes),
	append(FirstHalf, LastHalf, HashedCodes),
	same_length(FirstHalf, LastHalf),!,
        hex_codes_frac_float(FirstHalf, 1.0, LatFrac),
	hex_codes_frac_float(LastHalf, 1.0, LongFrac),
	grat_geo(LLat , LatFrac, PLat),
	grat_geo(LLong, LongFrac, PLong).



/**   globalhash(+Date:date,  -Point:point) is semidet

        find location of globalhash for this date
*/
 globalhash(Date, point(GLat, GLong)) :-
	hash_point(Date, graticule(10, -35), point(PLat, PLong)),
	GLat is abs(float_fractional_part(PLat)) * 180.0 - 90.0,
	GLong is abs(float_fractional_part(PLong)) * 360.0 - 180.0.


/**   grat_geo(+GratNum, +Frac, -Geo) is det

	 get a real geocoord from a graticule number and
	 fraction
*/

grat_geo(-0.0, Frac, Geo) :-
	!,
	Geo is -1.0 * Frac.
grat_geo(Num, Frac, Geo) :-
	Num >= 0.0,
	Geo is Num + Frac.
grat_geo(Num, Frac, Geo) :-
	Num < 0.0,
	Geo is Num - Frac.



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
	IntLong > -30,
	YYYY > 2008.
east_rule(YYYY - MM - _, IntLong) :-
	IntLong > -30,
	YYYY = 2008,
	MM > 5.
east_rule(YYYY - MM - DD, IntLong) :-
	IntLong > -30,
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


/*                     Test suite below here     */

%  main entry point for testing
test_geohashing :-
	retractall(croxstock:stock_stat(_,_,_,_)),
	retractall(a_hash_point(_,_,_)),!,
	test_hash_end_end,!,
	test_east_rule,!,
	test_grat_geo,!,
	test_hashatom,!,
	test_hex_frac,!,
	format('finished~n').

% test conversion from hex string fraction to float fraction
test_hex_frac :-
	test_case_hex_frac(Hex, Frac),
	call(hex_codes_frac_float(Hex, 1.0, GotFrac)),
	(   abs(GotFrac - Frac) < 0.0001 -> true
	;   format('hex_codes ~s  expected ~w got ~w~n', [Hex, Frac, GotFrac])
	),
	fail.
test_hex_frac.

test_case_hex_frac("db9318c2259923d0", 0.857713).
test_case_hex_frac("8b672cb305440f97", 0.544543).
test_case_hex_frac("ffffffffffffffff", 0.999999).
test_case_hex_frac("0000000000000000", 0.0).
test_case_hex_frac("8000000000000000", 0.5).

% test converting graticule and fractional location to geo coord
test_grat_geo :-
	test_case_gg(GratNum, Frac, Geo),
	call(grat_geo(GratNum, Frac, GotGeo)),
	(   GotGeo =:= Geo -> true
	;   format('grat_geo case ~w should be ~w~n', [grat_geo(GratNum, Frac, GotGeo), Geo])
	),
	fail.
test_grat_geo.

test_case_gg(40, 0.56, 40.56).
test_case_gg(40.0, 0.87, 40.87).
test_case_gg(0.0 , 0.66, 0.66).
test_case_gg(-0.0, 0.66, -0.66).
test_case_gg(-2.0, 0.45, -2.45).


% test the east rule rule
test_east_rule :-
	test_east_data(Dt, Long, true),
	east_rule(Dt, Long),
	(   east_rule(Dt, Long) ->
	    true
	;
	    format('eastrule should apply to ~w ~w but doesnt~n', [Dt, Long])
	),
	fail.
test_east_rule :-
	test_east_data(Dt, Long, false),
	(   east_rule(Dt, Long) ->
	    format('eastrule should not apply to ~w ~w but does~n', [Dt, Long])
	),
	fail.
test_east_rule.

test_east_data( 2008 - 5 - 20, 0, false).
test_east_data( 2013 - 5 - 28, -31, false).
test_east_data( 2013 - 5 - 28, 0, true).


% Test the entire hashpoint generation
test_hash_end_end :-
	test_item(Y-M-D, GLat, GLong, Lat, Long),
	hash_point(Y-M-D, graticule(GLat, GLong), point(PLat, PLong)),
	Dist is sqrt((Lat - PLat) * (Lat - PLat) +
		     (Long - PLong) * (Long - PLong)),
	(   Dist > 0.0001 ->         % 36 ft at equator
	    format('~w-~w-~w_~w_~w should be ~w,~w   but is ~w,~w~n',
		   [Y,M,D,GLat,GLong,Lat,Long,PLat,PLong])
	;   true),
	fail.
test_hash_end_end.

test_item(2013 - 5 - 31, 50, 8, 50.47615093, 8.75973636).
test_item(2013 - 5 - 31, 52, 1, 52.47615093, 1.75973636).
test_item(2013 - 5 - 31, 52, -0.0, 52.47615093, -0.75973636).
test_item(2013 - 5 - 31, 52, 0.0, 52.47615093, 0.75973636).
test_item(2013 - 5 - 31, 52, -1.0, 52.47615093, -1.75973636).
test_item(2013 - 6 - 6, 37, -120, 37.64757933, -120.17135344).
test_item(2013 - 6 - 6, 52, -2, 52.13259239, -2.18860213).
test_item(2013 - 6 - 6, 52, 2, 52.13259239, 2.18860213).

%	The 30W compliance test
test_item(2008 - 5 - 20, 68, -30, 68.63099, -30.61895 ).
test_item(2008 - 5 - 21, 68, -30, 68.17947, -30.86154 ).
test_item(2008 - 5 - 22, 68, -30, 68.97287, -30.2387 ).
test_item(2008 - 5 - 23, 68, -30, 68.40025, -30.72277 ).
test_item(2008 - 5 - 24, 68, -30, 68.12665, -30.54753 ).
test_item(2008 - 5 - 25, 68, -30, 68.94177, -30.18287 ).
test_item(2008 - 5 - 26, 68, -30, 68.67313, -30.60731 ).
test_item(2008 - 5 - 27, 68, -30, 68.20968, -30.10144 ).
test_item(2008 - 5 - 28, 68, -30, 68.68745, -30.21221 ).
test_item(2008 - 5 - 29, 68, -30, 68.4647, -30.03412 ).
test_item(2008 - 5 - 30, 68, -30, 68.8531, -30.2446 ).
test_item(2008 - 5 - 20, 68, -29, 68.63099, -29.61895 ).
test_item(2008 - 5 - 21, 68, -29, 68.17947, -29.86154 ).
test_item(2008 - 5 - 22, 68, -29, 68.97287, -29.2387 ).
test_item(2008 - 5 - 23, 68, -29, 68.40025, -29.72277 ).
test_item(2008 - 5 - 24, 68, -29, 68.12665, -29.54753 ).
test_item(2008 - 5 - 25, 68, -29, 68.94177, -29.18287 ).
test_item(2008 - 5 - 26, 68, -29, 68.67313, -29.60731 ).
test_item(2008 - 5 - 27, 68, -29, 68.12537, -29.57711 ).
test_item(2008 - 5 - 28, 68, -29, 68.71044, -29.11273 ).
test_item(2008 - 5 - 29, 68, -29, 68.27833, -29.74114 ).
test_item(2008 - 5 - 30, 68, -29, 68.32272, -29.70458 ).


test_hashatom :-
	hashatom_test_case(Dt, Grat, HashAtom),
	call(hash_atom(Dt, Grat, GotAtom)),
	(   GotAtom = HashAtom  -> true ;
		 format('hash_atom ~w ~w expeected ~q got ~q~n', [Dt, Grat, HashAtom, GotAtom])
	),
	fail.
test_hashatom.

% East rule still uses current date for string!
hashatom_test_case(2013 - 5 - 31, graticule(50, 8), '2013-05-31-15306.02').  % east rule
hashatom_test_case(2013 - 6 - 6 , graticule(37, -120), '2013-06-06-14955.45').

% peeron map has small 'toggle debug info' in LL corner. Very useful.
%
% some stock prices
% 2013-05-31  15322.22
% 2013-05-30  15306.02
% 2013-06-06  14955.45






