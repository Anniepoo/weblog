:- module(collection_demo, []).

/** <module> Collection demo

Generates an HTML demo page for **Weblog** collections (sets, tuples).

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2015
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(wl/format/wl_collection)).

:- http_handler(root(collection), collection_demo, [id(collection)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(collection, 'collections (sets, tuples)').



collection_demo(_):-
  Data = [a,b,[c,d,[]]],
  reply_html_page(
    wl_demo,
    title('Collections'),
    [
      h1('Collections'),

      h2('Weblog set'),
      p('Prolog code:'),
      \prolog_code(wl_set, Data),
      p('HTML rendering:'),
      \wl_set(Data),

      h2('Weblog tuple'),
      p('Prolog code:'),
      \prolog_code(wl_tuple, Data),
      p('HTML rendering:'),
      \wl_tuple(Data)
    ]
  ).

prolog_code(Pred, Data) -->
  {format(atom(Atom), '\\~a(~w)', [Pred,Data])},
  html(pre(Atom)).
