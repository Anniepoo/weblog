:- module(
  wl_table,
  [
    wl_direct_table//1, % +Data:list(list)
    wl_direct_table//2, % +Data:list(list)
                        % +Options:list
    wl_table//2
  ]
).

/** <module>  Utilities for laying out HTML tables

These are oriented towards presenting tables of data, not towards
controlling layout

@author Anne Ogborn
@author Wouter Beek
*/

:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(http/http_wrapper), [http_current_request/1]).

:- html_meta(direct_table_body(+,+,+,+,html,?,?)).
:- html_meta(direct_table_cells(+,+,html,?,?)).
:- html_meta(direct_table_header(+,+,html,-,?,?)).

:- meta_predicate(html_call(2,?,?)).
:- meta_predicate(html_call(3,+,?,?)).
:- meta_predicate(wl_direct_table(+,:,?,?)).

:- predicate_options(wl_direct_table//2, 2, [
     attributes(+list),
     caption(+callable),
     cell(+callable),
     indexed(+boolean),
     maximum_number_of_rows(nonneg)
   ]).

is_meta(caption).
is_meta(header).



%! wl_direct_table(+Data:list(list))// is semidet.
% @see wl_direct_table//2

wl_direct_table(Data) -->
  wl_direct_table(Data, []).

%! wl_direct_table(+Data:list(list), +Options:list)// is semidet.
% Given a list, each of whose members is either a list
% or a term head(List), outputs an HTML table representing the data.

% Upon encountering a head(List) term the elements of List are
% output as TH tags.
%
% Bare lists are output as TD tags,
%
% Fails silently if the data cannot be parsed.
%
% The generated HTML sets the class of body rows to even or odd
% alternately to allow alternate row styling.

wl_direct_table(Data0, Options0) -->
  {
    meta_options(is_meta, Options0, Options),
    option(attributes(Attrs), Options, []),
    option(caption(Caption), Options, _),
    option(cell(Cell), Options, html),
    option(indexed(Indexed), Options, false),
    option(maximum_number_of_rows(Max), Options, inf)
  }, !,
  html(
    table(Attrs, [
      \table_caption(Caption),
      \direct_table_header(Indexed, Data0, Cell, Data),
      tbody(\direct_table_body(1, Max, Indexed, Data, Cell))
    ])
  ).

direct_table_header(Indexed, [head(H)|T], Cell, T) --> !,
  html(
    thead(
      tr([
        \header_index_cell(Indexed),
        \direct_table_cells(header, H, Cell)
      ])
    )
  ).
direct_table_header(_, T, _, T) --> html([]).

direct_table_body(_, _, _, [], _) --> html([]).
direct_table_body(Row, Max, Indexed, [H|T], Cell) -->
  {
    Row @=< Max,
    (   0 =:= Row mod 2
    ->  EvenOdd = even
    ;   EvenOdd = odd
    ),
    NewRow is Row + 1
  },
  html(
    tr(class=EvenOdd, [
      \index_cell(Row, Indexed),
      \direct_table_cells(data, H, Cell)
    ])
  ),
  direct_table_body(NewRow, Max, Indexed, T, Cell).

direct_table_cells(_, [], _) --> html([]).
direct_table_cells(Tag, [H|T], Cell) -->
  (   {Tag == data}
  ->  html(td(\html_call(Cell, H)))
  ;   {Tag == header}
  ->  html(th(\html_call(Cell, H)))
  ),
  direct_table_cells(Tag, T, Cell).

:- html_meta wl_table(3, +, ?, ?).

:- predicate_options(wl_table/4, 2,
		     [ header(goal),
		       columns(list),
		       rows(list) ]).

/**	wl_table(+DataGen:goal, +OptionListIn:list)// is semidet

        Generate a table from a predicate that supplies the data
	as alternate solutions.

	DataGen is expected to be an arity 3 predicate
	my_data_gen(Key, Column, Value)

	wl_table outputs a HTML table showing the data
	for all possible solutions to pairs of Key and Column.

	Note that if you want the keys you'll have to make sure
	the key is included as a column name.

        Example, where *my_key*
	is the name of the primary key and my_data/3 is the underlying data.

	==
	...
        \wl_table(table_cells, [])
	...


	table_cells(Key, my_key, Key) :-
	      my_data(Key, _, _).
        table_cells(Key, Column, Value) :-
	      my_data(Key, Column, Value).
        ==

	The generated html sets the class of body
	rows to even or odd alternately to allow
	alternate row styling

	fails silently if given invalid arguments

	Options:
	* header(:Goal)
	Goal is expected to be an arity 2 predicate
	goal(Column, Label)
	if the option is missing, the column ID's are used
	as labels.
	if the option header(none)  is provided, no headers are
	produced
	Note that you'll have to specify the module explicitly

	* columns(+List)
	List is a list of column names. If this option exists,
	the columns in the list are displayed in the order given.
	Unless this list includes the key, it won't be included

        * rows(+List)
	List is a list of row names. If this option exists,
	the rows in the list are displayed in the order given.
*/
wl_table(DataGen, OptionListIn) -->
	{
	    meta_options(is_meta, OptionListIn, OptionList),
	    option(header(HeaderGoal) , OptionList, = ),
	    option(columns(OptionColumns), OptionList, true),
	    option(rows(OptionRows), OptionList, true),
	    findall(Key-Column, call(DataGen, Key, Column, _), Pairs),
	    pairs_keys_values(Pairs, DupKeyList, DupColumnList),
	    (	is_list(OptionColumns) ->
	        ColumnList = OptionColumns ;
	        list_to_set(DupColumnList, ColumnList)
	    ),
	    (	is_list(OptionRows) ->
	        KeyList = OptionRows ;
		list_to_set(DupKeyList, KeyList)
	    )
	},
	html([table([tr(\table_header(HeaderGoal, ColumnList)),
		     \table_body(1, KeyList, ColumnList, DataGen)])]).

:- html_meta table_header(2, +).

table_header(X, _) -->
	{
	   strip_module(X, _, none)
	},
	[],!.
table_header(_HeaderGoal, []) --> [].
table_header(HeaderGoal, [H|T]) -->
	{
	     call(HeaderGoal, H, Label)
	     ;
	     Label = H
	},
	html([th(Label), \table_header(HeaderGoal, T)]).

:- html_meta   table_body(+, +, +, 3).

table_body(_RowNum, [], _ColumnList, _DataGen) --> [].
table_body(RowNum, [H|T], ColumnList, DataGen) -->
	 {
	     NewRowNum is RowNum + 1,
	     (	 0 =:= RowNum mod 2
	     ->	 EvenOdd = even
	     ;	 EvenOdd = odd
	     )
	 },
	html(tr(class=EvenOdd, \table_row(H, ColumnList, DataGen))),
	table_body(NewRowNum, T, ColumnList, DataGen).

:- html_meta table_row(+, 3).

table_row(_, [], _) --> [].
table_row(Key, [H|T], DataGen) -->
	{
	    call(DataGen, Key, H, Value)
	},
	html(td(Value)),
	table_row(Key, T, DataGen).
table_row(Key, [H|T], DataGen) -->
	{
	    \+ call(DataGen, Key, H, _Value)
	},
	html(td('')),
	table_row(Key, T, DataGen).



% HELPERS %

%! header_index_cell(+Indexed:boolean)// is det.

header_index_cell(true) --> html(th('#')).
header_index_cell(false) --> html([]).


%! index_cell(+Row:nonneg, +Indexed:boolean)// is det.

index_cell(_, false) --> html([]).
index_cell(Row, true) --> html(td(Row)).


%! html_call(:Goal)// is det.

html_call(NoGoal) -->
  {\+ ground(NoGoal)}, !,
  html([]).
html_call(Goal, X, Y):-
  call(Goal, X, Y).


%! html_call(:Goal)// is det.

html_call(NoGoal, _) -->
  {\+ ground(NoGoal)}, !,
  html([]).
html_call(Goal, Arg, X, Y):-
  call(Goal, Arg, X, Y).


%! succ_inf(
%!   +X:or([nonneg,oneof([inf])]),
%!   -Y:or([nonneg,oneof([inf])])
%! ) is det.

succ_inf(inf, inf):- !.
succ_inf(X, Y):- succ(X, Y).


%! table_caption(:Caption)// is det.
% Generates the HTML table caption,
% where the content of the caption element is set by a DCG rule.
%
% @arg Caption A DCG rule generating the content of the caption element,
%              or uninstantiated, in which case no caption is generated.

table_caption(NoCaption) -->
  {\+ ground(NoCaption)}, !,
  html([]).
table_caption(Caption) -->
  html(caption(\html_call(Caption))).
