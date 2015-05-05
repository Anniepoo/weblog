:- module(menu_demo, []).

/**  <module> Menu demo

Generates an HTML demo page for  jQueryUI menus.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(wl/nav/wl_menu)).

:- http_handler(root(menu), menu_demo, [id(menu)]).

:- html_resource(css('demo.css'), []).

:- multifile(weblogdemo:label/2).
weblogdemo:label(menu, 'jQueryUI menus').



menu_demo(_) :-
	reply_html_page(wl_demo, title('Menu Demo'), \menu_demo_body).

menu_demo_body -->
	html([
	    \html_requires(css('demo.css')),
	    h1('Menu demo page'),
	    p('Menu widget using the jQueryui menu'),
	     h2('Menus for Fun and Profit!'),
	    \wl_menu('demo-menu'),
	    ul([id('demo-menu')], [
		   li([class='ui-state-disabled'],
		     span('A disabled item')),
		   li(a(href('http://www.swi-prolog.org'), 'SWI-Prolog')),
		   li([a(href='#', 'Questionable Food'),
		       ul([
			   li(a(href='http://www.spam.com', 'Spam')),
			   li(a(href='http://www.kraftrecipes.com/Products/ProductInfoDisplay.aspx?siteid=1&product=2100065883', 'Kraft Mac and Cheese')),
			   li([span('From Hormel'),
			       ul([
				   li(a(href='http://www.hormel.com/products/Variety.aspx?ID=14&RecipesOnly=False', 'Chili')),
				   li(a(href='http://www.hormel.com/products/Variety.aspx?ID=17&RecipesOnly=False#', 'Baco-bits'))
			       ])
			      ])
		       ])
		      ])
	         ])
	    ]).

