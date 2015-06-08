:- module(wl_menu, [wl_menu//1]).

/**  <module> WebLog menu

Support for the jQueryUI menu widget.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(wl/resource/wl_jquery_ui)).

:- html_resource(menu_css, [virtual(true), requires([css('menu.css')])]).



%! wl_menu(+ID)// is det
% Turns the object with id ID into a hierarchical menu.
%
% Usually ID is a `ul` item, and it turns the entire list into a
% hierarchical menu.
%
% ```prolog
% \wl_menu(mymenu),
% ul(id(mymenu), [
%   li(a(href(#), 'Menu 1')),
%   li(a(href(#), 'Has Submenu')),
%   ul([
%     li(a(href(#), 'Sub 1')),
%     li(a(href(#), 'Sub 2'))
%   ])
% ])
% ```

wl_menu(ID) -->
  html([
    \html_requires(wl_jquery_ui),
    \html_requires(menu_css),
    \html_post(head,
      \js_script({|javascript(ID)||
var agent;
$(function() {
  $("#"+ID).menu({
    position: {at: "left bottom"}
  });
});
      |})
    )
  ]).
