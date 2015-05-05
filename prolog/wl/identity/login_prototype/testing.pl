:- module(login_testing, []).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).

:- use_module(library(identity/login_prototype/login_box)).


% Just so we can read the pldocs
%
:- use_module(library(http/http_path)).
http:location(pldoc, root('help/source'), [priority(10)]).

:- doc_server(4040).

%% ============================================================ %%
%% =============================== webserver2_3 =============== %%
%% ============================================================ %%

% Declare a handler, binding an HTTP path to a predicate.
% Here our path is / (the root) and the goal we'll query will be
% say_hi. The third argument is for options
:- http_handler('/', say_hi, []).

% The predicate server(+Port) starts the server. It simply creates a
% number of Prolog threads and then returns to the toplevel, so you can
% (re-)load code, debug, etc.
server(Port) :-
        http_server(http_dispatch, [port(Port)]).

/* The implementation of /. The single argument provides the request
details, which we ignore for now. Our task is to write a CGI-Document:
a number of name: value -pair lines, followed by two newlines, followed
by the document content, The only obligatory header line is the
Content-type: <mime-type> header.
Printing is done with print_html, which takes a list of tokens and
prints them. It attempts to 'reasonably' format html when it recognizes
tags. */

say_hi(_Request) :-
	phrase(
	       html(html(
		[head(title('Howdy')),
		 body([h1('A Simple Web Page'),
		       div(class=container,[\login_box([])]),
		       p('With some text')])])),
	       TokenizedHtml,
	       []),
        format('Content-type: text/html~n~n'),
	print_html(TokenizedHtml).

%% ============================================================ %%
%% ============================================================ %%


:- http_handler('/control', control, []).

control(_Request):-
	(http_in_session(_), http_session_data(user(Name)))
	-> logged(Name)
	; (phrase(
	       html(html(
		[head(title('Sorry')),
		 body([h1('Members only')])])),
			 TokenizedHtml,
			 []),
	 format('Content-type: text/html~n~n'),
	 print_html(TokenizedHtml)
	).

logged(Name) :-
	phrase(
	       html(html(
		[head(title('Super Secret Page')),
		 body([h1(['Welcome ', Name])])])),
			 TokenizedHtml,
			 []),
	 format('Content-type: text/html~n~n'),
	 print_html(TokenizedHtml).


