:- module(
  html_list,
  [
    html_def_list//1, % +Definitions:list(pair)
    html_def_list//2, % +Definitions:list(pair)
                      % +Options:list
    html_list//1, % +Items:list
    html_list//2 % +Items:list
                 % +Options:list(nvpair)
  ]
).

/** <module> HTML list

Support for generating HTML lists.

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/html_write)).
:- use_module(library(option)).

:- use_module(library(support/html_meta)).

:- meta_predicate(html_def_list(+,:,?,?)).
:- meta_predicate(html_def_list_item(3,+,?,?)).
:- meta_predicate(html_def_list_items(3,+,?,?)).
:- meta_predicate(html_list(+,:,?,?)).
:- meta_predicate(html_list(+,3,+,?,?)).
:- meta_predicate(html_list_item(+,3,+,?,?)).
:- meta_predicate(html_list_items(+,3,+,?,?)).

is_meta(item_writer).



%! html_def_list(+Definitions:list)// is det.
% @see Wrapper around html_def_list//2 that uses the default
%      item writer html_pl_term//1.

html_def_list(L) -->
  html_def_list(L, []).

%! html_def_list(+Definitions:list(pair), +Options:list)// is det.
% Generates an HTML definition list.
%
% ### Arguments
%
% @arg Definitions A list of Prolog pairs that consist of a definiens
%      and a definiendum.
%      Both will be written by the supplied item writer
%      (see option `item_writer`).
% @Options A list of options.
%
% ### Options
%
% The following options are supported:
%   - item_writer(+callable)
%     A unary HTML DCG that writes the list elements.

html_def_list(L, Options0) -->
  {
    meta_options(is_meta, Options0, Options),
    option(item_writer(ItemWriter), Options, html)
  },
  html(dl(\html_def_list_items(ItemWriter, L))).

html_def_list_items(_, []) --> !, html([]).
html_def_list_items(ItemWriter, [H|T]) -->
  html_def_list_item(ItemWriter, H),
  html_def_list_items(ItemWriter, T).

html_def_list_item(ItemWriter, L) -->
  {is_list(L)}, !,
  html(dl(\html_def_list_items(ItemWriter, L))).
html_def_list_item(ItemWriter, def(Definiens,Definiendum)) -->
  html([
    dt(\html_call(ItemWriter, Definiens)),
    dd(\html_call(ItemWriter, Definiendum))
  ]).



%! html_list(+Items:list)// is det.
% @see Wrapper for html_list//3 using html_pl_term//1 as the item writer.

html_list(Items) -->
  html_list(Items, []).

%! html_list(+Items:list, +Options:list)// is det.
% Generates an HTML list containing the given list items.
%
% ### Arguments
%
% @arg Items A list of Prolog terms that will be written as the items
%      of an HTML list using the supplied item writer
%      (see option `item_writer`).
% @arg Options A list options.
%
% ### Options
%
% The following options are supported:
%   - item_writer(+callable)
%     A unary HTML DCG that writes the list elements.
%   - ordered(+boolean)
%     Whether the list is ordered (`OL`) or unordered (`UL`).
%     Default is `false`.

html_list(L, Options0) -->
  {
    meta_options(is_meta, Options0, Options),
    option(item_writer(ItemWriter), Options, html),
    option(ordered(Ordered), Options, false)
  },
  html_list(Ordered, ItemWriter, L).

html_list(false, ItemWriter, L) -->
  html(ul(\html_list_items(false, ItemWriter, L))).
html_list(true, ItemWriter, L) -->
  html(ol(\html_list_items(true, ItemWriter, L))).

html_list_items(_, _, []) --> !, html([]).
html_list_items(Ordered, ItemWriter, [H|T]) -->
  html_list_item(Ordered, ItemWriter, H),
  html_list_items(Ordered, ItemWriter, T).

html_list_item(Ordered, ItemWriter, L) -->
  {is_list(L)}, !,
  html(\html_list(Ordered, ItemWriter, L)).
html_list_item(_, ItemWriter, H) -->
  html(li(\html_call(ItemWriter, H))).
