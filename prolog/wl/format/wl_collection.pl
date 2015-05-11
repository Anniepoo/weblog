:- module(
  wl_collection,
  [
    wl_collection//4, % :Begin
                      % :End
                      % :ItemWriter
                      % +Items:list
    wl_pair//2, % +Element1
                % +Element2
    wl_pair//3, % :ItemWriter
                % +Element1
                % +Element2
    wl_pairs//1, % +Pairs:list
    wl_pairs//2, % :ItemWriter
                 % +Pairs:list
    wl_quadruple//4, % +Element1
                     % +Element2
                     % +Element3
                     % +Element4
    wl_quadruple//5, % :ItemWriter
                     % +Element1
                     % +Element2
                     % +Element3
                     % +Element4
    wl_set//1, % +Items:list
    wl_set//2, % :ItemWriter
               % +Items:list
    wl_triple//3, % +Element1
                  % +Element2
                  % +Element3
    wl_triple//4, % :ItemWriter
                  % +Element1
                  % +Element2
                  % +Element3
    wl_tuple//1, % +Items:list
    wl_tuple//2 % :ItemWriter
                % +Items:list
  ]
).

/** <module> Collections

DCG rules for generating HTML descriptions of collections such as
sets and tuples (including pairs, triples and quadruples).

The following class names are assigned (useful for styling):
  - collection
  - element

---

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2014-2015
*/

:- use_module(library(http/html_write)).
:- use_module(library(wl/support/html_meta)).

:- html_meta
  wl_collection(html,html,3,+,?,?),
  wl_collection(+,html,html,3,+,?,?),
  wl_collection_inner(+,html,html,3,+,?,?),
  wl_collection_item(+,html,html,3,+,?,?),
  wl_pair(3,+,+,?,?),
  wl_pairs(3,+,?,?),
  wl_quadruple(3,+,+,+,+,?,?),
  wl_set(3,+,?,?),
  wl_triple(3,+,+,+,?,?),
  wl_tuple(3,+,?,?).



%! wl_collection(:Begin, :End, :ItemWriter, +Items:list)// is det.

wl_collection(Begin, End, ItemWriter, L) -->
  wl_collection(collection, Begin, End, ItemWriter, L).

%! wl_collection(
%!   +Class:atom,
%!   :Begin,
%!   :End,
%!   :ItemWriter,
%!   +Items:list
%! )// is det.

wl_collection(Class, Begin, End, ItemWriter, L) -->
  html(
    span(
      class=Class,
      \html_between(
        Begin,
        wl_collection_inner(Class, Begin, End, ItemWriter, L),
        End
      )
    )
  ).

%! wl_collection_inner(
%!   +Class:atom,
%!   :Begin,
%!   :End,
%!   :ItemWriter,
%!   +Items:list
%! )// is det.

wl_collection_inner(_, _, _, _, []) --> !, html([]).
wl_collection_inner(Class, Begin, End, ItemWriter, [H]) --> !,
  wl_collection_item(Class, Begin, End, ItemWriter, H).
wl_collection_inner(Class, Begin, End, ItemWriter, [H|T]) -->
  html([
    \wl_collection_item(Class, Begin, End, ItemWriter, H),
    ',',
    \wl_collection_inner(Class, Begin, End, ItemWriter, T)
  ]).

wl_collection_item(Class, Begin, End, ItemWriter, H) -->
  {is_list(H)}, !,
  wl_collection(Class, Begin, End, ItemWriter, H).
wl_collection_item(_, _, _, ItemWriter, H) -->
  html(span(class=element, \html_call(ItemWriter, H))).



%! wl_pair(+Element1, +Element2)// is det.
% @see Wrapper around wl_pair//3 using the default element writer.

wl_pair(E1, E2) -->
  wl_pair(html, E1, E2).

%! wl_pair(:ItemWriter, +Element1, +Element2)// is det.
% Generates an HTML representation for a pair.

wl_pair(ItemWriter, E1, E2) -->
  wl_collection(pair, html(&(lang)), html(&(rang)), ItemWriter, [E1,E2]).



%! wl_pairs(+Pairs:list(pair))// is det.
% @see Wrapper around wl_pairs//2 using the default element writer.

wl_pairs(L) -->
  wl_pairs(html, L).

%! wl_pairs(:ItemWriter, +Pairs:list(pair))// is det.

wl_pairs(_, []) --> html([]).
wl_pairs(ItemWriter, [X-Y|T]) -->
  wl_pair(ItemWriter, X, Y),
  wl_pairs(ItemWriter, T).



%! wl_quadruple(+Element1, +Elememt2, +Element3, +Element4)// is det.
% @see Wrapper around wl_quadruple//5 using the default element writer.

wl_quadruple(E1, E2, E3, E4) -->
  wl_quadruple(html, E1, E2, E3, E4).

%! wl_quadruple(
%!   :ItemWriter,
%!   +Element1,
%!   +Elememt2,
%!   +Element3,
%!   +Element4
%! )// is det.
% Generates an HTML representation for a triple.

wl_quadruple(ItemWriter, E1, E2, E3, E4) -->
  wl_collection(
    quadruple,
    html(&(lang)),
    html(&(rang)),
    ItemWriter,
    [E1,E2,E3,E4]
  ).



%! wl_set(+Items:list)// is det.
% @see Wrapper around wl_set//2 using the default element writer.

wl_set(L) -->
  wl_set(html, L).

%! wl_set(:ItemWriter, +Items:list)// is det.

wl_set(ItemWriter, L) -->
  wl_collection(set, html(&(123)), html(&(125)), ItemWriter, L).



%! wl_triple(+Element1, +Element2, +Element3)// is det.
% @see Wrapper around wl_triple//4 that using the default element writer.

wl_triple(E1, E2, E3) -->
  wl_triple(html, E1, E2, E3).

%! wl_triple(:ItemWriter, +Element1, +Element2, +Element3)// is det.
% Generates an HTML representation for a triple.

wl_triple(ItemWriter, E1, E2, E3) -->
  wl_collection(
    triple,
    html(&(lang)),
    html(&(rang)),
    ItemWriter,
    [E1,E2,E3]
  ).



%! wl_tuple(+Items:list)// is det.
% @see Wrapper around wl_tuple//2 using the default element writer.

wl_tuple(L) -->
  wl_tuple(html, L).

%! wl_tuple(:ItemWriter, +Items:list)// is det.
% Generates an HTML representation for a tuple.

wl_tuple(ItemWriter, L) -->
  wl_collection(tuple, html(&(lang)), html(&(rang)), ItemWriter, L).
