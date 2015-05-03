:- module(link_demo, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(weblog(formatting/html_link)).

:- http_handler(root(link), link_page_handler, [id(link)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(link, 'Links').

link_page_handler(_):-
  reply_html_page(
    weblog_demo,
    title('Links'),
    [
      \html_image_link(
        'http://www.swi-prolog.org',
        'http://ecx.images-amazon.com/images/I/6136ED5EVCL.jpg'
      ),
      \html_external_link('http://www.wouterbeek.com')
    ]
  ).
