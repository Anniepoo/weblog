:- module(jquery_ui, []).

/** <module> jQueryUI

Defines the HTML resources for including jQueryUI into a Web page.

@author Anne Ogborn
@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2015/04
*/

:- use_module(library(http/html_head)).
:- use_module(library(wl/resource/jquery)).



:- html_resource(
  jquery_ui_css,
  [
    requires(['http://code.jquery.com/ui/1.11.4/themes/base/jquery-ui.css']),
    virtual(true)
  ]
).

:- html_resource(
  jquery_ui,
  [
    requires([
      jquery,
      jquery_ui_css,
      'http://code.jquery.com/ui/1.11.4/jquery-ui.js'
    ]),
    virtual(true)
  ]
).
