:- module(clippy_demo, []).
/** <module>  Demo page for Clippy

*/
% basic dispatch
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(weblog(widgets/agents/clippy)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_head)).

:- http_handler(root(clippy), clippy_demo_page, [id(clippy)]).

clippy_demo_page(_Request) :-
	reply_html_page(
	    title('Clippy Demo'),
	    \clippy_demo_body).

clippy_demo_body -->
	html([
	    \html_requires(jquery),
	    h1('Clippy'),
	    p('"Office XP is so easy to use that Clippy is no longer necessary, or useful," explained Lisa Gurry, a Microsoft product manager. "With new features like smart tags and Task Panes, Office XP enables people to get more out of the product than ever before. These new simplicity and ease-of-use improvements really make Clippy obsolete," she said.'),
	    \clippy(clippy_opts),
	    \js_script({| javascript(_) ||
function  dospeak() {
    agent.speak('I see you\'re writing a constraint programming system. Would you like some help with that?');
};
|}
		       ),
	    form(action='javascript:dospeak();', [
			    input([type=submit, name=submitt, value='Run Clippy'], [])
			]),
	    p(['Clippy-js is courtesy of the good folks at ', a(href='https://www.smore.com/clippy-js', 'Smore.com')]),
	    p('The agent variable is called agent. Documentation can be found at the above website.')
	     ]).

clippy_opts(character('Clippy')).

