:- module(
  google_font,
  [
    google_font//1 % +Name:atom
  ]
).

/** <module> Google font

Support for including Google fonts in Web pages.

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@see http://www.google.com/fonts
@version 2015
*/

:- use_module(library(http/html_write)).
:- use_module(library(uri)).



google_font(Name) -->
  {
    uri_query_components(Query, [family(Name)]),
    uri_components(
      Uri,
      uri_components(http,'fonts.googleapis.com','/css',Query,_)
    )
  },
  html(link([href=Uri,rel=stylesheet,type='text/css'], [])).
