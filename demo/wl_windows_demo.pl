:- module(wl_windows_demo, []).
/** <module> Demo page for windowing

*/
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

:- use_module(weblog(formatting/wl_windows)).


:- http_handler(root(wl_windows), windows_handler, [id(wl_windows)]).

windows_handler(_Request) :-
	reply_html_page(
	    title('Windows Demo'),
	    [
	       h1('Windows Demo'),
	       h2('Clickable Popup'),
	       \popup(click_popup_options,
		     p('click me'),
		     div(p('some random information'))),

	       h2('Rollover Popup'),
	       \popup(rollover_popup_options,
		     p('roll over me'),
		     div(p('other random info'))),
	       h2('Footnote Popup'),
		p(['This is a footnote.',
	            \popup(footnote_options,
		       '3',
		       'some misc info'),
		   'It seems academic.'])
	    ]).

click_popup_options(open_on(click)).

rollover_popup_options(open_time(100)).
rollover_popup_options(close_time(500)).
rollover_popup_options(close_method(bounce)).
footnote_options(footnote).



