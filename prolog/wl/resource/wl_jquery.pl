:- module(wl_jquery, []).

/** <module> Weblog version of jQuery

Defines the HTML resources for including jQuery into a Web page.

Not called `jquery` which is part of SWI-Prolog's HTTP library.

@author Anne Ogborn
@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2015
*/

:- use_module(library(http/html_head)).



:- html_resource(
  wl_jquery,
  [requires(['http://code.jquery.com/jquery-2.1.4.js']),virtual(true)]
).
