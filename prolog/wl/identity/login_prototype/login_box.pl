:-module(login_box, [login_box//1]).

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_dispatch)).

/** <module> Login Box WebLog Pattern

  This module implements predicates related to the login pattern.
  Closely related to user_database, register_box, logout_box.

  @see   http://www.welie.com/patterns/showPattern.php?patternID=login
  @tbd   https support
  @tbd   openid support (check openid swipl library)
*/

%% login_box(+Options) is det
%
%  A DCG producing a login box and implementing the basic functionality.
%  Options is a list of options that modify the default behavior.
%  The provided options are:
%
%     * after_login(+Atom)
%     logout_box: Displays a logout_box instead of a login_box when a user is logged in. Default.
%     logout_box(+Logout_Options): As logout_box but the logout_box is modified based on Logout_Options
%     destroy: Nothing will be displayed when a user is logged in
%     
%     * register(+RegisterPage)
%     Give the option to register by redirecting the user to the register page.
%     Deactivated by default
%     
%     * db_handler(+Database_Handler)
%     Provide a database of users (as described in the user_database module).
%     If not given, login_box will assume an empty database
%
%     * return(+SuccessPage, +FailPage)
%     After a succesful/failed login the user will be redirected to SuccessPage/FailPage
%     If the atom 'referer' is given, login_box will attempt to return the user to the previous page using the HTTP_REFERER information (default behavior)
%
%     * referer_check(+Check_Options)
%     A check to perform on the URL retrieved by HTTP_REFERER info before redirecting to it.
%     
%     +Check_Option has the form of {+Atom, +Mode}
%     Atom can be 'success', 'fail' or 'all'
%     Mode can be:
%     'false': no check will be performed
%     'empty': if the HTTP_REFERER field is empty the user is redirected to the root
%     empty(+ReturnPage): as 'empty' but the user is redirected to ReturnPage
%     'valid': the URL should appear to be a valid URL otherwise the user is redirected to the root (default)
%     valid(+ReturnPage): as 'valid' but the user is redirected to ReturnPage
%     predicate(:Check): a predicate to be called given the URL as an argument. If it fails, the user is redirected to the root
%     predicate(:Check, +ReturnPage): as predicate(:Check) but the user is redirected to ReturnPage
%     

login_box(_Options) --> 
	{http_set_session_options([create(noauto)]),
     http_handler('/user_login', login_request,[])},
	html([
	 form([action='/user_login',method='POST'],
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
	      ])]).

login_request(Request) :-
    member(referer(Referer),Request),
	http_read_data(Request, [name=Name,pass=Password|_], []),
	check_login(Name, Password, Details) ->
    succesful_login(Name, Referer, Request)
    ; failed_login(Details, Referer, Request).

succesful_login(Name, Return, Request) :-
	http_open_session(_SessionID, _),
	http_session_assert(user(Name)),
    http_redirect(see_other, Return, Request).
/*
	phrase(
	       html(html(
			 [head(title('Yay')),
			  body([p(['Welcome ', Name, ' ', Return])])])),
		    	    TokenizedHtml,
	    []),
        format('Content-type: text/html~n~n'),
	print_html(TokenizedHtml).
  */

failed_login(_Details, _Return, _Request) :-
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