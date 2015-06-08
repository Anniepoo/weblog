:- module(wl_jquery_ui, []).

/** <module> Weblog version of jQueryUI

Defines the HTML resources for including jQueryUI into a Web page.

@author Anne Ogborn
@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2015
*/

:- use_module(library(http/html_head)).
:- use_module(library(wl/resource/wl_jquery)).



:- html_resource(
  wl_jquery_ui_css,
  [
    requires(['https://code.jquery.com/ui/1.11.4/themes/ui-lightness/jquery-ui.css']),
    virtual(true)
  ]
).

:- html_resource(
  wl_jquery_ui,
  [
    requires([
      wl_jquery,
      wl_jquery_ui_css,
      'http://code.jquery.com/ui/1.11.4/jquery-ui.js'
    ]),
    virtual(true)
  ]
).
