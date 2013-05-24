/**  <module>  Maps display

    A  provider neutral maps component.

     Weblog
     Licensed under LGPL
*/

:- module(map,
	  [ geo_map_direct//2,			% +Coordinates
	    geo_map//2
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(settings)).

:- use_module(weblog(info/maps/google/gmap)).
:- use_module(weblog(info/maps/leaflet/leafletmap)).


/**	geo_map_direct(+Options, +Coordinates)// is det.

	HTML component that shows maps  with markers at the given
	Coordinates. Coordinates is a list. Each  coordinate is a
	term point(Lat,Long), optionally followed by decorations
	separated by + signs.

        Available Decorations:
	* popup(HTML)
	add a popup (small window with text that appears when clicking the
	location icon)
	* open
	The point must already have a popup. Makes the popup open by default.

	Options:
	* provider(ProviderName(ProviderSpecificOptionList))
	map can use one of several underlying map providers.
        Currently the choice is google or leaflet. The argument is a
	list of options which are only meaningful to
	that provider. At the moment there are no such options,
	and ProvideSpecificOptionList always binds to []


*/
:- predicate_options(geo_map_direct//2, 1, [
	provider(oneof([google(_), leaflet(_)])),
	id(text)
	       ]).

geo_map_direct(Options , Coordinates) -->
	{
	     option(provider(P), Options, google([])),
	     P =.. [google, ProviderArgs],!,
	     select(provider(_), Options, ProviderIndependentOptions),
	     append(ProviderArgs, ProviderIndependentOptions, PassOptions)
	},
	gmap(PassOptions, Coordinates).

geo_map_direct(Options , Coordinates) -->
	{
	     option(provider(P), Options),
	     P =.. [leaflet, ProviderArgs],!,
	     select(provider(_), Options, ProviderIndependentOptions),
	     append(ProviderArgs, ProviderIndependentOptions, PassOptions)
	},
	lmap(PassOptions, Coordinates).


geo_map_direct(Options , _Coordinates) -->
	{
		throw(error(domain_error(list, Options), context(geo_map_direct//2,
				   'invalid provider')))
	},
	[].


:- predicate_options(geo_map//2, 1, [
	provider(oneof([google(_), leaflet(_)])),
	id(text)
	       ]).

:- meta_predicate geo_map(+, 2, ?, ?).

geo_map(Options, Generator) -->
	{
	     option(provider(P), Options, google([])),
	     P =.. [google, ProviderArgs],!,
	     select(provider(_), Options, ProviderIndependentOptions),
	     append(ProviderArgs, ProviderIndependentOptions, PassOptions),
	     map_structure(Generator, Coordinates)
	},
	gmap(PassOptions, Coordinates).

geo_map(Options, Generator) -->
	{
	     option(provider(P), Options),
	     P =.. [leaflet, ProviderArgs],!,
	     select(provider(_), Options, ProviderIndependentOptions),
	     append(ProviderArgs, ProviderIndependentOptions, PassOptions),
	     map_structure(Generator, Coordinates)
	},
	lmap(PassOptions, Coordinates).



geo_map(Options, _Generator) -->
	{
		throw(error(domain_error(list, Options), context(geo_map//2,
				   'invalid provider')))
	},
	[].

:- meta_predicate  map_structure(2, -).

map_structure(Generator, Coordinates) :-
	bagof(X, call(Generator, X, _), RawList),
	convert_structure(RawList, Coordinates).

convert_structure([], []).
convert_structure([map|HT], T) :-
	convert_structure(HT, T).
convert_structure([point(X, Y)| HT], [point(X, Y)| T]) :-
	convert_structure(HT, T).
