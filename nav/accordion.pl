:- module(accordion, [accordion//1, acc_header//1]).
/** <module>   Accordion widget



*/
:- use_module(library(http/html_write)).


% making the first arg html means swipl
% will enforce that it's well formed html
%
% But we don't want this. It's actually a list of
% elements that need expansion. It could be worse - could
% be an arbitrary structure whose elements need expansion or
% don't
%
:- meta_predicate accordion(:, ?, ?).


%%	accordion(+Items:list)// is det
%
% for the moment I'm not actually generating
% the accordion javascript stuff, just divs with classes
% that alternate
%
%    @param Items alternating list of header, body
%
accordion(_:[]) --> [].
accordion(M:[H,B|T]) -->
	{
	    debug(weblog, 'accordion got ~q: ~q', [M, [H,B|T]] )
	},
	M:html([
	    div(class=[accordion,head], H),
	    div(class=[accordion,body],B)
	     ]),
	accordion(M:T).
accordion(X) -->
	{
	    debug(weblog, 'accordion fallback got ~q', [X] )
	},
       [].

:- html_meta acc_header(html, ?, ?).

acc_header(Contents) -->
	html([
	    div(class=accheader, Contents)
	     ]).




