/**  <module>  Maps display

    A  provider neutral maps component.

     Weblog
     Licensed under LGPL
*/

:- module(map,
	  [ geo_map//2			% +Coordinates
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(settings)).

:- use_module(weblog(info/maps/google/gmap)).
:- use_module(weblog(info/maps/leaflet/leafletmap)).


/**	map(+Options, +Coordinates)// is det.

	HTML component that shows maps  with markers at the given
	Coordinates. Coordinates is a list. Each  coordinate is a
	term point(Lat,Long). This can be extended by defining
	more gmap:coordinate/3 clauses

	Options:
	* provider(ProviderName(ProviderSpecificOptionList))
	map can use one of several underlying map providers. Currently the choice is
	google or leaflet. The argument is a list of options which are only meaningful to
	that provider. For example, leaflet allows named styles.


*/
:- predicate_options(geo_map//2, 1, [
	provider(oneof([google(_), leaflet(_)])),
	id(text)
	       ]).

geo_map(Options , Coordinates) -->
	{
	     option(provider(P), Options, google([])),
	     P =.. [google, ProviderArgs],!,
	     select(provider(_), Options, ProviderIndependentOptions),
	     append(ProviderArgs, ProviderIndependentOptions, PassOptions)
	},
	gmap(PassOptions, Coordinates).

geo_map(Options , Coordinates) -->
	{
	     option(provider(P), Options),
	     P =.. [leaflet, ProviderArgs],!,
	     select(provider(_), Options, ProviderIndependentOptions),
	     append(ProviderArgs, ProviderIndependentOptions, PassOptions)
	},
	lmap(PassOptions, Coordinates).


geo_map(Options , _Coordinates) -->
	{
		throw(error(domain_error(list, Options), context(geo_map//1,
				   'invalid provider')))
	},
	[].
