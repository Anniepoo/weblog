:- module(
  html_collection,
  [
    html_collection//4, % :Begin
                        % :End
                        % :ItemWriter
                        % +Items:list
    html_pair//2, % +Element1
                  % +Element2
    html_pair//3, % :ItemWriter
                  % +Element1
                  % +Element2
    html_pairs//1, % +Pairs:list
    html_pairs//2, % :ItemWriter
                   % +Pairs:list
    html_quadruple//4, % +Element1
                       % +Element2
                       % +Element3
                       % +Element4
    html_quadruple//5, % :ItemWriter
                       % +Element1
                       % +Element2
                       % +Element3
                       % +Element4
    html_set//1, % +Items:list
    html_set//2, % :ItemWriter
                 % +Items:list
    html_triple//3, % +Element1
                    % +Element2
                    % +Element3
    html_triple//4, % :ItemWriter
                    % +Element1
                    % +Element2
                    % +Element3
    html_tuple//1, % +Items:list
    html_tuple//2 % :ItemWriter
                  % +Items:list
  ]
).

/** <module> HTML collections

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

:- use_module(library(support/html_meta)).

:- meta_predicate(html_pair(3,+,+,?,?)).
:- meta_predicate(html_pairs(3,+,?,?)).
:- meta_predicate(html_quadruple(3,+,+,+,+,?,?)).
:- meta_predicate(html_set(3,+,?,?)).
:- meta_predicate(html_triple(3,+,+,+,?,?)).
:- meta_predicate(html_tuple(3,+,?,?)).

:- html_meta(html_collection(html,html,3,+,?,?)).
:- html_meta(html_collection(+,html,html,3,+,?,?)).
:- html_meta(html_collection_inner(+,html,html,3,+,?,?)).
:- html_meta(html_collection_item(+,html,html,3,+,?,?)).



%! html_collection(:Begin, :End, :ItemWriter, +Items:list)// is det.

html_collection(Begin, End, ItemWriter, L) -->
  html_collection(collection, Begin, End, ItemWriter, L).

%! html_collection(
%!   +Class:atom,
%!   :Begin,
%!   :End,
%!   :ItemWriter,
%!   +Items:list
%! )// is det.

html_collection(Class, Begin, End, ItemWriter, L) -->
  html(
    span(
      class=Class,
      \html_between(
        Begin,
        html_collection_inner(Class, Begin, End, ItemWriter, L),
        End
      )
    )
  ).

%! html_collection_inner(
%!   +Class:atom,
%!   :Begin,
%!   :End,
%!   :ItemWriter,
%!   +Items:list
%! )// is det.

html_collection_inner(_, _, _, _, []) --> !, html([]).
html_collection_inner(Class, Begin, End, ItemWriter, [H]) --> !,
  html_collection_item(Class, Begin, End, ItemWriter, H).
html_collection_inner(Class, Begin, End, ItemWriter, [H|T]) -->
  html([
    \html_collection_item(Class, Begin, End, ItemWriter, H),
    ',',
    \html_collection_inner(Class, Begin, End, ItemWriter, T)
  ]).

html_collection_item(Class, Begin, End, ItemWriter, H) -->
  {is_list(H)}, !,
  html_collection(Class, Begin, End, ItemWriter, H).
html_collection_item(_, _, _, ItemWriter, H) -->
  html(span(class=element, \html_call(ItemWriter, H))).



%! html_pair(+Element1, +Element2)// is det.
% @see Wrapper around html_pair//3 using the default element writer.

html_pair(E1, E2) -->
  html_pair(html, E1, E2).

%! html_pair(:ItemWriter, +Element1, +Element2)// is det.
% Generates an HTML representation for a pair.

html_pair(ItemWriter, E1, E2) -->
  html_collection(pair, html(&(lang)), html(&(rang)), ItemWriter, [E1,E2]).



%! html_pairs(+Pairs:list(pair))// is det.
% @see Wrapper around html_pairs//2 using the default element writer.

html_pairs(L) -->
  html_pairs(html, L).

%! html_pairs(:ItemWriter, +Pairs:list(pair))// is det.

html_pairs(_, []) --> html([]).
html_pairs(ItemWriter, [X-Y|T]) -->
  html_pair(ItemWriter, X, Y),
  html_pairs(ItemWriter, T).



%! html_quadruple(+Element1, +Elememt2, +Element3, +Element4)// is det.
% @see Wrapper around html_quadruple//5 using the default element writer.

html_quadruple(E1, E2, E3, E4) -->
  html_quadruple(html, E1, E2, E3, E4).

%! html_quadruple(
%!   :ItemWriter,
%!   +Element1,
%!   +Elememt2,
%!   +Element3,
%!   +Element4
%! )// is det.
% Generates an HTML representation for a triple.

html_quadruple(ItemWriter, E1, E2, E3, E4) -->
  html_collection(quadruple, html(&(lang)), html(&(rang)), ItemWriter, [E1,E2,E3,E4]).



%! html_set(+Items:list)// is det.
% @see Wrapper around html_set//2 using the default element writer.

html_set(L) -->
  html_set(html, L).

%! html_set(:ItemWriter, +Items:list)// is det.

html_set(ItemWriter, L) -->
  html_collection(set, html(&(123)), html(&(125)), ItemWriter, L).



%! html_triple(+Element1, +Element2, +Element3)// is det.
% @see Wrapper around html_triple//4 that using the default element writer.

html_triple(E1, E2, E3) -->
  html_triple(html, E1, E2, E3).

%! html_triple(:ItemWriter, +Element1, +Element2, +Element3)// is det.
% Generates an HTML representation for a triple.

html_triple(ItemWriter, E1, E2, E3) -->
  html_collection(
    triple,
    html(&(lang)),
    html(&(rang)),
    ItemWriter,
    [E1,E2,E3]
  ).



%! html_tuple(+Items:list)// is det.
% @see Wrapper around html_tuple//2 using the default element writer.

html_tuple(L) -->
  html_tuple(html, L).

%! html_tuple(:ItemWriter, +Items:list)// is det.
% Generates an HTML representation for a tuple.

html_tuple(ItemWriter, L) -->
  html_collection(tuple, html(&(lang)), html(&(rang)), ItemWriter, L).
