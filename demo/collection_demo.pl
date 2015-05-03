:- module(collection_demo, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(weblog(formatting/html_collection)).

:- http_handler(root(collection), collection_page_handler, [id(collection)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(collection, 'Collection terms (sets, tuples)').

collection_page_handler(_):-
  Data = [a,b,[c,d,[]]],
  reply_html_page(
    weblog_demo,
    title('Collections'),
    [
      h1('Collections'),

      h2('Set'),
      p('Prolog code:'),
      \prolog_code(html_set, Data),
      p('HTML rendering:'),
      \html_set(Data),

      h2('Tuple'),
      p('Prolog code:'),
      \prolog_code(html_tuple, Data),
      p('HTML rendering:'),
      \html_tuple(Data)
    ]
  ).

prolog_code(Pred, Data) -->
  {format(atom(Atom), '\\~a(~w)', [Pred,Data])},
  html(pre(Atom)).
