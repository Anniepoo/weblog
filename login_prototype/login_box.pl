:-module(login_box, [login_box/2]).


:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).

login_box(LoginBoxTokenized, Tail) :-
	http_set_session_options([create(noauto)]),
	http_handler('/user_login', login_request,[]),
	html([
	 form([action='/user_login', method='POST'],
	      [
	       p([], [
		      label([for=name],'Username:'),
		      input([name=name, type=textarea])
		     ]),
	       p([], [
		      label([for=pass],'Password:'),
		      input([name=pass, type=password])
		     ]),
	       p([], input([name=submit, type=submit, value='Submit'], []))
	      ])], LoginBoxTokenized, Tail).

login_request(Request) :-
	http_read_data(Request, [name=Name,pass=Password|_], []),
	check_login(Name, Password, Details) -> succesful_login(Name) ; failed_login(Details).

succesful_login(Name) :-
	http_open_session(_SessionID, _),
	http_session_assert(user(Name)),
	phrase(
	       html(html(
			 [head(title('Yay')),
			  body([p(['Welcome ', Name])])])),
		    	    TokenizedHtml,
	    []),
        format('Content-type: text/html~n~n'),
	print_html(TokenizedHtml).

failed_login(_Details) :-
	phrase(
	       html(html(
			 [head(title(':(')),
			  body([p('The username or password you entered is incorrect.')])])),
	       TokenizedHtml,
	       []),
        format('Content-type: text/html~n~n'),
	print_html(TokenizedHtml).

check_login(Name, Password, true) :-
	% delay in each attempted login to reduce brute-force attack efficiency
	sleep(1),
	% retrieving account details (user table key = username)
	user_db(Name, UserPassword, Salt),
	% first hash
	sha_hash(Password, Hash1, [algorithm(sha512)]),
	hash_atom(Hash1, HashAtom1),
	% second hash with salt
	atom_concat(HashAtom1, Salt, Salted),
	sha_hash(Salted, Hash, [algorithm(sha512)]),
	hash_atom(Hash, UserPassword).

user_db(thanosqr, '766286d68e742b693e7d712a434cab5b9775cb11bde8a7285a09642a220d269029c75df7d624c76bc76a972afce92e7427876cee650273ad9a02a04ff0d061a0', random).
user_db(annie, '94db72db39580e0559ad6224358e58791b4eeb2511750e759c8b1f24de79a52b2e8abda02ac43097587bee3a5b63f42f41beb236dad4193e7bb40f45259aa04a', iswear).

% we never store the passwords in plaintext
% 42 42
% oops