:- module(listbox_demo, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(library(formatting/html_listbox)).

:- http_handler(root(listbox), listbox_page_handler, [id(listbox)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(listbox, 'Listbox').

listbox_page_handler(_):-
  Data = [option(a,'A',false),option(b,'B',true),option(c,'C',false)],
  reply_html_page(
    weblog_demo,
    title('Listbox'),
    [
      h1('Listbox'),
      p('Prolog code:'),
      \prolog_code(Data),
      p('HTML rendering:'),
      \html_listbox(Data)
    ]
  ).

prolog_code(Data) -->
  {format(atom(Atom), '\\html_listbox(~w)', [Data])},
  html(pre(Atom)).
