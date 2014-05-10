:- module(ajaxify_demo, []).
/** <module> Demo page for ajaxify, a tool to allow arbitrary reload of chunks of html

*/
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).


:- use_module(weblog(html_form/ajaxify)).
:- use_module(weblog(support/utils)).
:- reexport(weblog(support/utils), [wl_opts/2]).

:- http_handler(root(ajaxify), ajaxify_demo_page, [id(ajaxify)]).

ajaxify_demo_page(_Request) :-
	reply_html_page(
	    title('Ajaxify Demo'),
	    \ajaxify_demo_body).

ajaxify_demo_body -->
	html([
	    h1('Ajaxify Demo'),
	    form([action='#'],[
		  p('This paragraph was loaded normally'),
		  p([\ajaxify(wl_opts(time), p(\server_time))])
		     % well that was dumber than a dirt sandwich!
		     % guess I better just document it
		 ])
	]).

server_time -->
	{
             get_time(T),
	     format_time(atom(Time), '%A, %B %e, %I:%M:%3f', T)
        },
	html(p(Time)).

