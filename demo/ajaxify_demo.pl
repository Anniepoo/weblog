:- module(ajaxify_demo, []).
/** <module> Demo page for ajaxify, a tool to allow arbitrary reload of chunks of html

*/
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_wrapper)).


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
	    p('This paragraph was loaded normally'),
	    p([\ajaxify(utils:wl_opts([timer(2000), id(demoajax)]),
			p(\server_time))]),
		     % well that was dumber than a dirt sandwich!
		     % guess I better just document it
	    form([action='#'],[
		     \ajaxify_contents(demoajax, ajaxsender),
		     \ajaxify_broadcast(demotext, return,
			  input([id(ajaxsender), type(textarea)], []))
		 ]),
	    h1('Uppercase'),
	    \ajaxify_contents(demotext, ajaxsender),
	    \ajaxify(utils:wl_opts([timer(2000), id(demotext)]), p(\show_text))
	]).

show_text -->
	{
	     http_current_request(Request),
             http_parameters(Request, [
			ajaxsender(Val, [ optional(true),
                                          default('') ])
			     ]),
             upcase_atom(Val, UVal)
        },
	html([p(UVal)]).

server_time -->
	{
	     http_current_request(Request),
             http_parameters(Request, [
			ajaxsender(Val, [ optional(true),
                                          default('') ])
			     ]),
             get_time(T),
	     format_time(atom(Time), '%A, %B %e, %I:%M:%3S', T)
        },
	html([p(Time), p(Val)]).
