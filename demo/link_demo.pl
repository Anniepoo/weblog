:- module(link_demo, []).

/** <module> Link demo

Generates an HTML demo page for **Weblog** links.

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2015
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(wl/format/wl_link)).

:- http_handler(root(link), link_demo, [id(link)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(link, 'links').



link_demo(_):-
  reply_html_page(
    wl_demo,
    title('Links'),
    [
      \wl_image_link(
        'http://www.swi-prolog.org',
        'http://ecx.images-amazon.com/images/I/6136ED5EVCL.jpg'
      ),
      \wl_external_link('http://www.wouterbeek.com')
    ]
  ).
