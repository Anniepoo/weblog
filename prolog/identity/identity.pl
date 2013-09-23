:- module(identity, [
	  logged_in_as//3,
	  logout_uri/2,
	  login_uri/2
	  ]).
/** <module> non-visual predicates for managing identity

     Use this module in preference to directly calling the provider
     identity management

*/

:- use_module(weblog(identity/stormpath/stormpath)).
:- use_module(library(http/http_dispatch)).

logged_in_as(Generator, Name, Role) -->
	{
	    logged_in_as(Generator, Name, Role)
        },
	[].

logged_in_as(Generator, Name, Role) :-
        call(Generator, provider(stormpath)),
	stormpath:logged_in_as(Generator, Name, Role).

default_login_uri('/login').
default_logout_uri('/logout').

login_uri(Generator, URI) :-
       call(Generator, login_uri(URI)),
       ensure_handler_exists(login, URI).
login_uri(_, URI) :-
	default_login_uri(URI),
	ensure_handler_exists(login, URI).

logout_uri(Generator, URI) :-
       call(Generator, logout_uri(URI)),
       ensure_handler_exists(logout, URI).
logout_uri(_, URI) :-
	default_logout_uri(URI),
	ensure_handler_exists(logout, URI).


ensure_handler_exists(login, _) :- !.
ensure_handler_exists(login, URI) :-
	http_handler(URI, login_handler , []).

login_handler(Request) :-
	reply_html_page(weblog_login,
			title('Log in'),
			\login_form
		       ).


