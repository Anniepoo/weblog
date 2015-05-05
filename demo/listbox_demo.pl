:- module(listbox_demo, []).

/** <module> Listbox demo

Generates an HTML demo page for listboxes.

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2015
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(wl/format/wl_listbox)).

:- http_handler(root(listbox), listbox_demo, [id(listbox)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(listbox, 'listbox').



listbox_demo(_):-
  Data = [option(a,'A',false),option(b,'B',true),option(c,'C',false)],
  reply_html_page(
    wl_demo,
    title('Listbox'),
    [
      h1('Listbox'),
      p('Prolog code:'),
      \prolog_code(Data),
      p('HTML rendering:'),
      \wl_listbox(Data)
    ]
  ).

prolog_code(Data) -->
  {format(atom(Atom), '\\wl_listbox(~w)', [Data])},
  html(pre(Atom)).
