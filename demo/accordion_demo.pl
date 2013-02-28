:- module(accordion_demo, []).
/** <module>  Demo handler for accordion

*/
% basic dispatch
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(weblog(nav/accordion)).

:- http_handler(root(accordion), accordion_demo_page, [id(accordion)]).

accordion_demo_page(_Request) :-
	reply_html_page(
	    title('Accordion Demo'),
	    \acc_demo_body).

acc_demo_body -->
	html([
	    h1('Accordion demo page'),
	     p('Tasty Recipes, Too Many For One Page!'),
	    \accordion([collapsible(true)], [
		 \acc_section('Taco Salad',
		         \recipe(taco_salad)),
		 \acc_section('Grilled Cheese Sandwich',
		         \recipe(grilled_cheese)),
		 \acc_section('Tang',
		     div([
			p('ingredients:'),
		        ul([
			  li('1 Tbspn Tang powder'),
			  li('20 oz cold water')
		          ]),
		        p('Mix Tang powder into cold water. Serve')
		        ]))
	         ])
	    ]).

recipe(ID) -->
	{
	    setof(li(Ingredient), ingredient(ID, Ingredient), Ingredients),
	    procedure(ID, Procedure)
	},
	html([
	    div([
		p('ingredients:'),
		ul(Ingredients),
		p('Procedure:'),
		ul(Procedure)
		])
	     ]).


ingredient(taco_salad,  '12" flour tortilla').
ingredient(taco_salad, '6 oz iceberg lettuce').
ingredient(taco_salad, '6 oz hamburger').
ingredient(taco_salad, 'Durkees taco seasonings').
ingredient(taco_salad, 'grated mixed cheddar and Mozarella cheese').
ingredient(taco_salad, '2oz Salsa Fresca').
ingredient(grilled_cheese, 'white or sourdough bread').
ingredient(grilled_cheese, 'American cheese, pre-wrapped slices').
procedure(taco_salad,
'Place tortilla over flat bottom wire strainer and fry for 15 seconds to make
bowl. Mix hamburger with taco seasonings and fry. Chop lettuce into 1" cubes and separate. Mix lettuce with half of hamburger and cheese and place in bowl.
Add remaining ingredients to top.').
procedure(grilled_cheese,
'Place 3 slices cheese between bread slices. Heat sandwich on dry griddle until cheese melts, turning halfway').
