:- module(wl_windows, [popup//3]).
/** <module> Tools for making windows in the browser

*/
:- use_module(library(http/html_write)).
% :- use_module(library(option)).
% :- use_module(library(http/http_wrapper), [http_current_request/1]).
:- use_module(library(http/html_head)).

:- html_meta popup(1, html, html, ?, ?).

popup(Options, Activator, Popup) -->
	{
           call(Options, footnote)
        },
	html([
	    \html_requires(wl_window),
	    \html_requires(wl_window_css),
	    sup(class=[fn, footnote], [Activator, span(class(fnp), span(class(footcontain), Popup))])
	]).
popup(_, _, _) -->
	html([
	    p('missing required options in popup')
	]).
