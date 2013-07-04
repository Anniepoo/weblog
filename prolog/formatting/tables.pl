:- module(wl_table, [wl_direct_table//1,
		     wl_table//2]).
/** <module>  Utilities for laying out HTML tables

These are oriented towards presenting tables of data, not towards
controlling layout

*/

:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(http/http_wrapper), [http_current_request/1]).

/**	wl_direct_table(+Data:listoflists)// is semidet

	Given a list, each of whose members is either a list
	or a term head(List), outputs an HTML table representing
	the data.

	Upon encountering a head(List) term the elements of List are
	output in th tags.

	Bare lists are output as td tags

	fails silently if it can't parse the data

	The generated html sets the class of body
	rows to even or odd alternately to allow
	alternate row styling
*/
wl_direct_table([]) --> [].
wl_direct_table([H|T]) -->
         html([table(\direct_table_body(1, [H|T]))]).

direct_table_body(_, []) --> [].
direct_table_body(RowNum, [H|T]) -->
	 {
	     is_list(H),
	     NewRowNum is RowNum + 1,
	     (	 0 =:= RowNum mod 2
	     ->	 EvenOdd = even
	     ;	 EvenOdd = odd
	     )
	 },
         html(tr(class=EvenOdd, \direct_table_cells(td, H))),
	 direct_table_body(NewRowNum, T).
direct_table_body(RowNum, [head(HR)|T]) -->
	direct_header_row(HR),
	direct_table_body(RowNum, T).

direct_header_row([]) --> [].
direct_header_row([H|T]) -->
	html(tr([\direct_table_cells(th, [H|T])])).

direct_table_cells(_, []) --> [].
direct_table_cells(Tag, [H|T]) -->
	{
	   Cell =.. [Tag, H]
	},
	html([Cell]),
	direct_table_cells(Tag, T).

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

	wl_table outputs an HTML table showing the data
	for all possible solutions to Key, Column

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


%
%  which options require module resolution
%
is_meta(header).


