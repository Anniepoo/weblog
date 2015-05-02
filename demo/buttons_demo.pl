:- module(buttons_demo, []).
/** <module>  Demo handler for buttons

*/
% basic dispatch
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(weblog(info/buttons/buttons)).
:- use_module(weblog(formatting/wl_table)).

:- http_handler(root(buttons), buttons_demo_page, [id(buttons)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(buttons, 'Buttons').

buttons_demo_page(_Request) :-
	reply_html_page(weblog_demo, title('Buttons Demo'), \buttons_demo_body).

buttons_demo_body -->
	html([
	    style(
		'.stylename  {
		      font-family: monospace;
			     }'),
	    h1('Button Widgets'),
	    p('Small controls that interact with various social web sites'),
	    h2('Reddit'),
	    h3(['The ', span(class=stylename, tiny), ' style']),
	    \reddit([style(tiny)]),
	    h3(['The ', span(class=stylename, plus), ' style']),
	    \reddit([style(plus)]),
	    h3(['The ', span(class=stylename, score_only), ' style']),
	    \reddit([style(score_only)]),
	    h3(['The ', span(class=stylename, tiny_score), ' style']),
	    \reddit([style(tiny_score)]),
	    h3(['The ', span(class=stylename, tiny_vote), ' style']),
	    \reddit([style(tiny_vote)]),
	    h2('del.icio.us'),
	    \delicious([site_name('Buttons Demo')])
	     ]).

