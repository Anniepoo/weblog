:- module(
  html_listbox,
  [
    html_listbox//1, % +Items:list(compound)
    html_listbox//2 % +Items:list(compound)
                    % +Options:list
  ]
).

/** <module> HTML drop-down list

Support for generating HTML drop-down lists.

@author Wouter Beek
@version 2014-2015
*/

:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- use_module(weblog(formatting/html_pl_term)).
:- use_module(weblog(support/html_meta)).

:- predicate_options(html_listbox//2, 2, [
     attributes(+list),
     item_writer(+callable)
   ]).

:- meta_predicate(html_listbox(+,:,?,?)).
:- meta_predicate(html_options(3,+,?,?)).

is_meta(item_writer).



%! html_listbox(+Items:list(compound))// is det.
% @see Wrapper around html_listbox//2 with default options.

html_listbox(L) -->
  html_listbox(L, []).

%! html_listbox(+Items:list(compound), +Options:list)// is det.
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

html_listbox(L, Options0) -->
  {
    meta_options(is_meta, Options0, Options),
    option(attributes(Attrs0), Options, []),
    option(item_writer(ItemWriter), Options, html_option),
    (   option(multiple(_), Attrs0)
    ->  Attrs = Attrs0
    ;   selectchk(option(_,_,true), L, L0),
        memberchk(option(_,_,true), L0)
    ->  merge_options([multiple(true)], Attrs0, Attrs)
    ;   merge_options([multiple(false)], Attrs0, Attrs)
    )
  },
  html(select(Attrs, \html_options(ItemWriter, L))).

% html_options(:ItemWriter, +Items:list(compound))// is det.

html_options(_, []) --> !, html([]).
html_options(Goal, [H|T]) -->
  html_call(Goal, H),
  html_options(Goal, T).

%! html_option(+Item:compound)// is det.

html_option(option(Value,Label,Selected)) -->
  html(option([selected=Selected,value=Value], Label)).
