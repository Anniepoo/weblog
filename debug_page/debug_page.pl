:- module(debug_page, []).
/**   <module>  Debugging Page

Displays generally useful debug information.

 At the moment does little, eventually should

 * List of registered handlers and paths
 * Current File Search Path
 * Current Session info
 * Info from
 [[stats.pl]]

*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
/*
:- use_module(library(http/http_session)).
:- use_module(weblog(formatting/boxes)).
*/

:- http_handler(admin(debugpage) , debug_page_handler, [id(debug_page)]).

debug_page_handler(_Request) :-
	reply_html_page(
	    hhp_web_style,
	    title('HHP Virtual Web App Debugging Page'),
	    debug_contents).

% dont use abox here, want to keep this separated
debug_contents -->
	html([p('not much here at moment')]).

	/*
	\(layout:abox('table', 'Session Info',
		     \debug_page:all_session_info)),
	\(layout:abox('table', 'All File Handlers',
		     \debug_page:all_handlers)),
	\(layout:abox('table', 'Current File Search Path',
		     \debug_page:current_fsp))
		     http_dispatch:handler(Path, Pred, true, Options)
		     */


