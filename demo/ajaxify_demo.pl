:- module(ajaxify_demo, []).

/** <module> Ajaxify demo

Generates an HTML demo page for ajaxify, a tool that allows
arbitrary chunks of HTML to be reloaded.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(wl/form/ajaxify)).
:- use_module(library(wl/support/wl_util)).

:- http_handler(root(ajaxify), ajaxify_demo_page, [id(ajaxify)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(ajaxify, 'ajaxify').

ajaxify_demo_page(_Request) :-
	reply_html_page(wl_demo, title('Ajaxify Demo'), \ajaxify_demo_body).

ajaxify_demo_body -->
	html([
	    h1('Ajaxify Demo'),
	    p('This paragraph was loaded normally'),
	    p([\ajaxify(wl_opts([timer(5000), id(demoajax)]),
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
	    \ajaxify(wl_opts([id(demotext)]), p(\show_text))
	]).

show_text -->
	{
	     http_current_request(Request),
             http_parameters(Request, [
			ajaxsender(Val, [ optional(true) ])
			     ]),
             upcase_atom(Val, UVal)
        },
	html([p(UVal)]).

server_time -->
	{
	     http_current_request(Request),
             http_parameters(Request, [
			ajaxsender(Val, [ optional(true) ])
			     ]),
             get_time(T),
	     format_time(atom(Time), '%A, %B %e, %I:%M:%3S', T)
        },
	html([p(Time), p(Val)]).
