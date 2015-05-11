:- module(ajaxify, [ajaxify//2,
		    ajaxify_broadcast//3,
		    ajaxify_contents//2]).

/** <module> Ajaxify

Turn any HTML generation into ajax.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(lists)).
:- use_module(library(wl/resource/jquery)).

:- html_meta
  ajax_wrapper(1, html, +),
  ajaxify(1, html, ?, ?),
  ajaxify_broadcast(+, html),
  ensure_ajax_handler_exists(1, +, html).

:- html_resource(
  ajaxify_base,
  [requires([jquery,js('wlajaxify.js')]),virtual(true)]
).



%! ajaxify(+Generator:goal, +HTML:html, ?A:list, ?B:list) is det
%	Ajaxify turns its contents into a `div` element whose content is
%	replaced by the second argument, called in an ajax call,
%	when signalled by ajax.
%
% ### Arguments for Generator
%
%	Generator takes the following arguments:
%   
%	  * id(ID) 
%   Binds this handler to a specific `id` value of a `div` element.
%	  Note that this is a global namespace, so `mypage_mything`
%   is recommended.
%   This argument is required to be supported by Generator.
%   
%   * listen_to(Name)
%   Listen to all, ID, and anything that matches Name.
%   
%   * timer(Speed)
%   Call this ajaxify every Speed milliseconds.
%
% ### Error handling
%
% In the event of an error, the contents of `#error` will be
% appended with the error message

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



ajaxify_broadcast(Name, return, HTML) -->
  {
    outer_id(HTML, ID, OHTML),
    ground(ID),  % make sure we really have it
    atomic_concat('#', ID, PID)
  },
  html(OHTML),
  html(\js_script(
       {|javascript(PID, Name)||
	    $(PID).keyup(function(event) {
	        if ( event.which == 13 ) {
		    ajaxify.talk(Name);
                    event.preventDefault();
                }
	    });
       |}
   )).

%! outer_id(+HTML, ?ID, -OHTML)
% Make sure we have a matching ID in the OHTML.

outer_id(input(A, C), ID, input(A, C)) :-
	outer_id([input(A, C)], ID, [input(A, C)]).
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
   html(\js_script(
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
       !,
       (   Speed  > 99 ;
           throw(error(bad_idea(too_fast_ajax),
		       context(ajaxify/4, 'Timer ajax faster than 100msec')))),
       call(Generator, id(ID)),
       ajax_path_name(Generator, AjaxPath)
   },
   html(\js_script(
       {|javascript(Speed, ID, AjaxPath)||
	    ajaxify.tick(Speed, ID, AjaxPath);
       |}
   )).
timer_register(_) -->[].


ajax_path_name(Generator, AjaxPath) :-
	call(Generator, id(ID)),
	http_current_request(Request),
	member(path(Path), Request),
	atomic_list_concat([Path, '/ajax/', ID], AjaxPath).


ensure_ajax_handler_exists(_, AjaxPath, _) :-
	http_dispatch:handler(AjaxPath, _, _, _),!.
ensure_ajax_handler_exists(Generator, AjaxPath, HTML) :-
	http_handler(AjaxPath, ajax_wrapper(Generator, HTML), []).


ajax_wrapper(Generator, HTML, _Request) :-
	format('Content-type: text/html~n'),
	call(Generator, id(ID)),
	format('X-Clear: 1~n'),
	format('X-Id: ~w~n~n', [ID]),
	phrase(html(HTML), Tokens),
	print_html(Tokens).

%! ajaxify_contents(+Message, +Contents, ?A, ?B) is det
%	Add contents of an element to ajaxify calls.
%
%	@arg Message When you send this message
%	@arg Contents Add this element to the query string, with id the
%	     element name.

ajaxify_contents(Message, Contents) -->
   html(\js_script(
       {|javascript(Message, Contents)||
	    ajaxify.add_contents(Message, Contents);
       |}
   )).
