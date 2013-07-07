:- module(menu, [wl_menu//1]).
/**  <module>  Tools for the jqueryui menu widget

*/
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/js_write)).

:- ensure_loaded(weblog(resources/resources)).

/**     wl_menu(+ID)// is det

Turns the object with id ID into a hierarchical menu.

Usually ID is a ul item, and it turns the entire list into a
hierarchical menu.

     ==

     \wl_menu(mymenu),
     ul(id(mymenu), [
	    li(a(href(#), 'Menu 1')),
	    li([a(href(#), 'Has Submenu'),
		ul([
		    li(a(href(#), 'Sub 1')),
		    li(a(href(#), 'Sub 2'))
		])])])

     ==


*/

wl_menu(ID) -->
	html([
	    \html_requires(jquery_ui),
	    \html_requires(menu_css),
	    \html_post(head,
		       \js_script({| javascript(ID) ||
var agent;
$(function() {
    $( "#"+ID ).menu({
                position: {at: "left bottom"}
            });
});
|}
))
	]).

