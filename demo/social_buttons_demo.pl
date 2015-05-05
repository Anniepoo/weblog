:- module(social_buttons_demo, []).

/** <module> Social buttons demo

Generates an HTML demo page for social buttons.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(wl/info/social/social_button)).
:- use_module(library(wl/format/wl_table)).

:- http_handler(root(social_button), social_button_demo, [id(social_button)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(social_button, 'social buttons').



social_button_demo(_Request) :-
	reply_html_page(
    wl_demo,
    title('Social buttons demo'),
    \social_button_demo_body
  ).

social_button_demo_body -->
	html([
	    style(
		'.stylename  {
		      font-family: monospace;
			     }'),
	    h1('Button Widgets'),
	    p('Small controls that interact with various social web sites'),
	    h2('Reddit'),
	    h3(['The ', span(class=stylename, tiny), ' style']),
	    \reddit([style(tiny)]),
	    h3(['The ', span(class=stylename, plus), ' style']),
	    \reddit([style(plus)]),
	    h3(['The ', span(class=stylename, score_only), ' style']),
	    \reddit([style(score_only)]),
	    h3(['The ', span(class=stylename, tiny_score), ' style']),
	    \reddit([style(tiny_score)]),
	    h3(['The ', span(class=stylename, tiny_vote), ' style']),
	    \reddit([style(tiny_vote)]),
	    h2('del.icio.us'),
	    \delicious([site_name('Buttons Demo')])
	     ]).

