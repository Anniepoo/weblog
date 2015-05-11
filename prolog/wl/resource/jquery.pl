:- module(jquery, []).

/** <module> jQuery

Defines the HTML resources for including jQuery into a Web page.

@author Anne Ogborn
@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2015/04
*/

:- use_module(library(http/html_head)).



:- html_resource(
  jquery,
  [requires(['http://code.jquery.com/jquery-2.1.4.js']),virtual(true)]
).
