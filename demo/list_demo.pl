:- module(list_demo, []).

/** <module> list demo

Generates an HTML demo page for HTML lists.

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2015
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(wl/format/wl_list)).

:- http_handler(root(list), list_demo, [id(list)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(list, 'list').

list_demo(_):-
  Data = [a,b,[c,d,[]],e,f],
  reply_html_page(
    wl_demo,
    title('List'),
    [
      h2('Definition list'),
      p('Prolog code:'),
      pre('\\wl_def_list([def(a,b),def(1,2),[def(x,y)]])'),
      p('HTML rendering:'),
      \wl_def_list([def(a,b),def(1,2),[def(x,y)]]),
      
      h2('List'),
      p('Prolog code:'),
      \prolog_code(Data),
      p('HTML rendering:'),
      \wl_list(Data)
    ]
  ).

prolog_code(Data) -->
  {format(atom(Atom), '\\wl_list(~w)', [Data])},
  html(pre(Atom)).
