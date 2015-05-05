:- module(
  wl_list,
  [
    wl_def_list//1, % +Definitions:list(pair)
    wl_def_list//2, % +Definitions:list(pair)
                    % +Options:list
    wl_list//1, % +Items:list
    wl_list//2 % +Items:list
               % +Options:list(nvpair)
  ]
).

/** <module> Weblog list

Support for generating HTML lists.

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(wl/support/html_meta)).

:- meta_predicate
  wl_def_list(+,:,?,?),
  wl_def_list_item(3,+,?,?),
  wl_def_list_items(3,+,?,?),
  wl_list(+,:,?,?),
  wl_list(+,3,+,?,?),
  wl_list_item(+,3,+,?,?),
  wl_list_items(+,3,+,?,?).

is_meta(item_writer).



%! wl_def_list(+Definitions:list)// is det.
% @see Wrapper around wl_def_list//2 that uses the default
%      item writer wl_pl_term//1.

wl_def_list(L) -->
  wl_def_list(L, []).

%! wl_def_list(+Definitions:list(pair), +Options:list)// is det.
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

wl_def_list(L, Options0) -->
  {
    meta_options(is_meta, Options0, Options),
    option(item_writer(ItemWriter), Options, html)
  },
  html(dl(\wl_def_list_items(ItemWriter, L))).

wl_def_list_items(_, []) --> !, html([]).
wl_def_list_items(ItemWriter, [H|T]) -->
  wl_def_list_item(ItemWriter, H),
  wl_def_list_items(ItemWriter, T).

wl_def_list_item(ItemWriter, L) -->
  {is_list(L)}, !,
  html(dl(\wl_def_list_items(ItemWriter, L))).
wl_def_list_item(ItemWriter, def(Definiens,Definiendum)) -->
  html([
    dt(\html_call(ItemWriter, Definiens)),
    dd(\html_call(ItemWriter, Definiendum))
  ]).



%! wl_list(+Items:list)// is det.
% @see Wrapper for wl_list//3 using wl_pl_term//1 as the item writer.

wl_list(Items) -->
  wl_list(Items, []).

%! wl_list(+Items:list, +Options:list)// is det.
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

wl_list(L, Options0) -->
  {
    meta_options(is_meta, Options0, Options),
    option(item_writer(ItemWriter), Options, html),
    option(ordered(Ordered), Options, false)
  },
  wl_list(Ordered, ItemWriter, L).

wl_list(false, ItemWriter, L) -->
  html(ul(\wl_list_items(false, ItemWriter, L))).
wl_list(true, ItemWriter, L) -->
  html(ol(\wl_list_items(true, ItemWriter, L))).

wl_list_items(_, _, []) --> !, html([]).
wl_list_items(Ordered, ItemWriter, [H|T]) -->
  wl_list_item(Ordered, ItemWriter, H),
  wl_list_items(Ordered, ItemWriter, T).

wl_list_item(Ordered, ItemWriter, L) -->
  {is_list(L)}, !,
  html(\wl_list(Ordered, ItemWriter, L)).
wl_list_item(_, ItemWriter, H) -->
  html(li(\html_call(ItemWriter, H))).
