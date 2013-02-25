:- module(accordion, [accordion//1, acc_header//1]).
/** <module>   Accordion widget

*/
:- use_module(library(http/html_write)).


% making the first arg html means swipl
% will enforce that it's well formed html
%
% But we really want well formed args to
% html//1
:- html_meta accordion(+, ?, ?).


%%	accordion(+Items:list)// is det
%
% for the moment I'm not actually generating
% the accordion javascript stuff, just divs with classes
% that alternate
%
%    @param Items alternating list of header, body
%
accordion([]) --> [].
accordion([H,B|T]) -->
	{
	    debug(weblog, 'Accordion sees ~q', [[H,B|T]])
	},
	html([
	    div(class=[accordion,head], H),
	    div(class=[accordion,body],B)
	     ]),
	accordion(T).

:- html_meta acc_header(html, ?, ?).

acc_header(Contents) -->
	html([
	    div(class=accheader, Contents)
	     ]).




