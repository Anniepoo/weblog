:- module(table_demo, []).

/** <module> Demo page for tables

*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

:- use_module(weblog(formatting/wl_table)).

:- http_handler(root(wl_table), table_handler, [id(wl_table)]).

table_handler(_Request) :-
	reply_html_page(
	    title('Table Demo'),
	    [
	     style(
'tr.even td, tr.even {
	     background-color: #aaaaff;
	    }'),
	     h1('Table Demo'),
	     p('Table from nested list data'),
	     \wl_direct_table([
		 head(['Name', 'Quiz1', 'Quiz2', 'Midterm', 'Final']),
		 ['Abigail Ames', 73, 84, 92, 87],
		 ['Bob Burns', 23, 45, 77, 45],
		 ['Charlie Clark', 99, 100, 89, 94]
		      ]),
	     hr([]),
	     p('Table From Facts'),
	     \wl_table(grades_table_cells, []),
	     hr([]),
	     p('Table From Facts with Column Names'),
	     \wl_table(grades_table_cells, [header(table_demo:grade_labels)]),
	     hr([]),
	     p('Table From Facts without Header'),
	     \wl_table(grades_table_cells, [header(none)]),
	     hr([]),
	     p('Table From Facts with explicit columns'),
	     \wl_table(grades_table_cells, [columns([name, quiz1, final])]),
	     hr([]),
	     p('Table From Facts with explicit rows'),
	     \wl_table(grades_table_cells, [rows(['Arnie Adams', 'Brenda Burns'])])
	    ]).

grade_labels(name, 'Student').
grade_labels(quiz1, 'Quiz 1').
grade_labels(quiz2, 'Quiz 2').
grade_labels(midterm, 'Midterm').
grade_labels(final, 'Final').

%  Note that we're styling the text before returning
%
%  NOTE - subtlety here. The order of the answers is important,
%  which is non logical.  The -> is needed to avoid leaving
%  a choice point in a bad place.
%

grades_table_cells(Name, name, b(Name)) :- grade(Name, _, _).
grades_table_cells(Name, Assignment, RetValue) :-
	grade(Name, Assignment, Value),
	(   Value < 60
	->  RetValue = em(Value)
	;   RetValue = Value
	).

grade('Arnie Adams', quiz1, 45).
grade('Arnie Adams', quiz2, 65).
grade('Arnie Adams', midterm, 85).
grade('Arnie Adams', final, 45).

grade('Brenda Burns', quiz1, 83).
grade('Brenda Burns', quiz2, 85).
grade('Brenda Burns', midterm, 95).
grade('Brenda Burns', final, 95).

grade('Cindy Cameo', quiz1, 40).
%   missing value
grade('Cindy Cameo', midterm, 39).
grade('Cindy Cameo', final, 29).

grade('Dwight Dangerman', quiz1, 78).
grade('Dwight Dangerman', quiz2, 98).
grade('Dwight Dangerman', midterm, 85).
grade('Dwight Dangerman', final, 90).


%
% temporary - trying to find a clean way to
% handle pagination
%
:- http_handler(root(next), next_handler, [id(next)]).

next_handler(Request) :-
	http_parameters(Request, [
				  page(Page, [integer, default(0)])
				 ]),
	Next is Page + 1,
	reply_html_page(title('Next!'),
			[
			 p('Your number is ' , Page),
			 a(href=location_by_id(next_handler) + [page(Next)],
			   'Next')
			]).



