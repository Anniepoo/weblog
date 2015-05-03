:- module(list_demo, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(weblog(formatting/html_list)).

:- http_handler(root(list), list_page_handler, [id(list)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(list, 'List').

list_page_handler(_):-
  Data = [a,b,[c,d,[]],e,f],
  reply_html_page(
    weblog_demo,
    title('List'),
    [
      h2('Definition list'),
      p('Prolog code:'),
      pre('\\html_def_list([def(a,b),def(1,2),[def(x,y)]])'),
      p('HTML rendering:'),
      \html_def_list([def(a,b),def(1,2),[def(x,y)]]),
      
      h2('List'),
      p('Prolog code:'),
      \prolog_code(Data),
      p('HTML rendering:'),
      \html_list(Data)
    ]
  ).

prolog_code(Data) -->
  {format(atom(Atom), '\\html_list(~w)', [Data])},
  html(pre(Atom)).
