:- module(clippy_demo, []).

/** <module> Clippy demo

Generates an HTML demo page for the Clippy agent.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/js_write)).
:- use_module(library(wl/resource/jquery)).
:- use_module(library(wl/widget/agent/clippy)).

:- http_handler(root(clippy), clippy_demo_page, [id(clippy)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(clippy, 'Clippy').



clippy_demo_page(_Request) :-
	reply_html_page(wl_demo, title('Clippy demo'), \clippy_demo_body).

clippy_demo_body -->
	html([
	  \html_requires(jquery),
	  h1('Clippy'),
	  p('"Office XP is so easy to use that Clippy is no longer necessary, or useful," explained Lisa Gurry, a Microsoft product manager. "With new features like smart tags and Task Panes, Office XP enables people to get more out of the product than ever before. These new simplicity and ease-of-use improvements really make Clippy obsolete," she said.'),
	  \clippy(clippy_opts),
	  \js_script({|javascript(_)||
function dospeak() {
  clippy.load('Clippy', function(agent) {
    agent.show();
    agent.speak('I see you\'re writing a constraint programming system. Would you like some help with that?');
  })
};
    |}),
	  form(action='javascript:dospeak();',
		  input([type=submit, name=submitt, value='Run Clippy'], [])
	  ),
	  p(['Clippy-js is courtesy of the good folks at ', a(href='https://www.smore.com/clippy-js', 'Smore.com')]),
	  p('The agent variable is called agent. Documentation can be found at the above website.')
	]).

clippy_opts(character('Clippy')).
