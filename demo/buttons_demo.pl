:- module(buttons_demo, []).
/** <module>  Demo handler for buttons

*/
% basic dispatch
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(weblog(info/buttons/buttons)).
:- use_module(weblog(formatting/tables)).

:- http_handler(root(buttons), buttons_demo_page, [id(buttons)]).

buttons_demo_page(_Request) :-
	reply_html_page(
	    title('Buttons Demo'),
	    \buttons_demo_body).

buttons_demo_body -->
	html([
	    h1('Button Widgets'),
	    h2('Reddit'),
	    \reddit([style(tiny)]),
	    \reddit([style(plus)]),
	    \reddit([style(score_only)]),
	    \reddit([style(tiny_score)]),
	    \reddit([style(tiny_vote)])
	     ]).



