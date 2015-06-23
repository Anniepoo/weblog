:- module(
  wl_listbox,
  [
    wl_listbox//1, % +Items:list(compound)
    wl_listbox//2 % +Items:list(compound)
                  % +Options:list
  ]
).

/** <module> Listboxes

Support for generating HTML drop-down lists.

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2014-2015
*/

:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(wl/support/html_meta)).

:- meta_predicate
  wl_listbox(+,:,?,?),
  wl_options(3,+,?,?).

:- predicate_options(wl_listbox//2, 2, [
  attributes(+list),
  item_writer(+callable)
]).

is_meta(item_writer).



%! wl_listbox(+Items:list(compound))// is det.
% @see Wrapper around wl_listbox//2 with default options.

wl_listbox(L) -->
  wl_listbox(L, []).

%! wl_listbox(+Items:list(compound), +Options:list)// is det.
% Generates an HTML listbox.
% Items have the following form:
%
% ```prolog
% option(+Value:atom, +Label:atom, +Selected:boolean)
% ```
%
% ### Options
%
% The following options are supported:
%   - attributes(+list)
%     HTML attributes of the SELECT element.
%   - item_writer(+callable)
%     The HTML DCG rules that is used to generate each item.

wl_listbox(L, Options0) -->
  {
    meta_options(is_meta, Options0, Options),
    option(attributes(Attrs0), Options, []),
    option(item_writer(ItemWriter), Options, wl_option),
    (   option(multiple(_), Attrs0)
    ->  Attrs = Attrs0
    ;   selectchk(option(_,_,true), L, L0),
        memberchk(option(_,_,true), L0)
    ->  merge_options([multiple(true)], Attrs0, Attrs)
    ;   Attrs = Attrs0
    )
  },
  html(select(Attrs, \wl_options(ItemWriter, L))).

% wl_options(:ItemWriter, +Items:list(compound))// is det.

wl_options(_, []) --> !, html([]).
wl_options(Goal, [H|T]) -->
  html_call(Goal, H),
  wl_options(Goal, T).

%! wl_option(+Item:compound)// is det.

wl_option(option(Value,Label,Selected)) -->
  html(option([selected=Selected,value=Value], Label)).
