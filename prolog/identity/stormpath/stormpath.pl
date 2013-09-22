:- module(stormpath, []).
/** <module> Access to Stormpath, an identity management system

   Stormpath [http://www.stormpath.com/] is a commercial service
   that offloads the hassle of user management.

   @tbd stop accepting any cert

*/
:- use_module(library(http/http_header)).  % needed for POST
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

cert_verify(_SSL, ProblemCert, _AllCerts, _FirstCert, _Error) :-
        format(user_error, 'Accepting certificate ~w~n', [ProblemCert]).


base_uri('https://api.stormpath.com/v1').

with_base_uri(RelURI, AbsURI) :-
	atom_concat('/', _, RelURI), !,
	base_uri(Base),
	atom_concat(Base, RelURI, AbsURI).
with_base_uri(RelURI, AbsURI) :-
	base_uri(Base),
	atomic_list_concat([Base, '/', RelURI], AbsURI).

% todo add exception handling
stormpath_post(RelURI, JSONIn, JSONOut) :-
	with_base_uri(RelURI, AbsURI),
	setting(stormpath_user, stormpath(Name, PassWord, _)),
	prolog_to_json(JSONIn, JSONInJSON),
	atom_json_term(In, JSONInJSON, []),
        http_open(AbsURI, Stream,
                  [ cert_verify_hook(cert_verify),
		    authorization(Name, PassWord),
		    method(post),
		    data(In)
                  ]),
	json_read(Stream, JSONOutJSON),
	json_to_prolog(JSONOutJSON, JSONOut),
	close(Stream).

stormpath_get(RelURI, JSONOut) :-
	with_base_uri(RelURI, AbsURI),
	setting(stormpath_user, stormpath(Name, PassWord, _)),
        http_open(AbsURI, Stream,
                  [ cert_verify_hook(cert_verify),
		    authorization(Name, PassWord),
		    method(get)
                  ]),
	json_read(Stream, JSONOutJSON),
	json_to_prolog(JSONOutJSON, JSONOut),
	close(Stream).

