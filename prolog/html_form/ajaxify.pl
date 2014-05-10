:- module(ajaxify, [ajaxify//2, ajaxify_broadcast//3]).
/** <module> Turn any html generation into ajax

*/

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(weblog(resources/resources)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/js_write)).


:- html_meta  ajaxify(1, html, ?, ?).

%%	ajaxify(+Generator:goal, +HTML:html, ?A:list, ?B:list) is det
%
%	ajaxify turns its contents into a div whose contents is
%	replaced by the second argument, called in an ajax call,
%	when signalled by ajax.
%
%	arguments for Generator
%
%	id(ID)      required - binds this handler to a specific ID.
%	                       this will also be the id of the div
%	                       note that this is a global namespace, so
%	                       mypage_mything is recommended
%      listen_to(Name)         listen to all, ID, and anything
%                              that matches Name

ajaxify(Generator, HTML) -->
   {
       call(Generator, id(ID)),
       ajax_path_name(Generator, AjaxPath),
       ensure_ajax_handler_exists(Generator, AjaxPath, HTML)
   },
   html(div(id(ID), [&(nbsp)])),
   html_requires(ajaxify_base),
   register_listener(load, Generator),
   register_listener(all, Generator),
   generator_register(Generator),
   timer_register(Generator),
   register_listener(ID, Generator).

:- html_meta ajaxify_broadcast(+, html).


ajaxify_broadcast(Name, return, HTML) -->
   {
       outer_id(HTML, ID, OHTML),
       ground(ID),  % make sure we really have it
       atomic_concat('#', ID, PID)
   },
   html(OHTML),
   html(js_script(
       {|javascript(PID, Name)||
	    $(PID).keypress(function(event) {
	        if ( event.which == 13 ) {
		    ajaxify.talk(Name)
                    event.preventDefault();
                }
	    }
       |}
   )).

outer_id([input(A, C) | T], ID, [input(A, C) | T]) :-
	is_list(A),
	member(id(ID), A).
outer_id([input(A, C) | T], ID, [input(A, C) | T]) :-
	is_list(A),
	member(id=ID , A).
outer_id([input(A, C) | T], ID, [input([id(ID) | A], C) | T]) :-
	is_list(A),
	\+ memberchk(id(_) , A),
	\+ memberchk(id=_ , A),
	ground(ID).
outer_id([input(A, C) | T], ID, [input([id(ID) , A], C) | T]) :-
	\+ is_list(A),
	A \= id(_),
	ground(ID).

register_listener(Name, Generator) -->
   {
       call(Generator, id(ID)),
       ajax_path_name(Generator, AjaxPath)
   },
   html(js_script(
       {|javascript(Name, ID, AjaxPath)||
	    ajaxify.listen(Name, ID, AjaxPath);
       |}
   )).

generator_register(Generator) -->
	{
            findall(X, call(Generator, listen_to(X)), List)
        },
	generator_register_(Generator, List).

generator_register_(_, []) --> [].
generator_register_(Generator, [H|T]) -->
	register_listener(H, Generator),
	generator_register_(T, Generator).

timer_register(Generator) -->
   {
       call(Generator, timer(Speed)),
       call(Generator, id(ID)),
       ajax_path_name(Generator, AjaxPath)
   },
   html(js_script(
       {|javascript(Speed, ID, AjaxPath)||
	    ajaxify.tick(Speed, ID, AjaxPath);
       |}
   )).


ajax_path_name(Generator, AjaxPath) :-
	call(Generator, id(ID)),
	http_current_request(Request),
	member(path(Path), Request),
	atomic_list_concat([Path, '/ajax/', ID], AjaxPath).

:- html_meta ensure_ajax_handler_exists(1, +, html).

ensure_ajax_handler_exists(_, AjaxPath, _) :-
	http_dispatch:handler(AjaxPath, _, _, _),!.
ensure_ajax_handler_exists(Generator, AjaxPath, HTML) :-
	http_handler(AjaxPath, ajax_wrapper(Generator, HTML), []).

:- html_meta ajax_wrapper(1, html, +).

ajax_wrapper(_Generator, HTML, _Request) :-
	format('Content-type: text/html\n\n'),
	phrase(html(HTML), Tokens),
	print_html(Tokens).



