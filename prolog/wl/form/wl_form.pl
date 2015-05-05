:- module(
  wl_form,
  [
			validated_form/2,
			error_message//2,
			form_field//3,
			form_invalidate/0,
			length_input_minmax/5,
			numeric_minmax/5,
			value/4
  ]
).

/**  <module> HTML form validator

This module handles the common web task of creating a form,
validating the input, and, if not valid, redirecting the user
back to the form with error messages over the offending elements

   So, say the form is
==
   name: [       ]
   age:  [     ]
   (Submit)
==

   The user enters their name but leaves their age blank.

   they next see
==
   name:  [Sally Smith]
   You need to enter an age:
   age:   [    ]
   (Submit)
==

   They enter their age and click submit.  They land on
   a landing page:


==
   Thank you for your personal info. We'll be sure
   to enter it in our database and as a bonus we'll
   make sure it gets entered in many others throughout
   the globe.
        (Home)
==

   To implement this example we would define a handler

==
:- http_handler(root(spamform) , spam_page_handler, [id(spamform)]).

spam_page_handler(Request) :-
	validated_form(
	    reply_html_page(
		web_style,
		[title('A little problem....')],
		login_form(Request)),
	    reply_html_page(
		web_style,
		[title('Thanks'),
		spam_landing_page_content(Request))).
==

and then in login_form you do something like

==
login_form(Request) -->
 ....
      form([action='/spamform', method=POST], [
	     \(html_form:error_message([for=name], html(p([class=oops], 'you have to type a name')))),
	     \(html_form:form_field(Request, length_input_minmax(3, '>'), input([name=name, type=textarea], []))),
	     \(html_form:error_message([for=age], p([class=oops], 'Problem with age'))),
	     \(html_form:form_field(Request, numeric_minmax(14, '>'), input([name=age, type=textarea], [])))
					       ....]
==

@author Anne Ogborn
@copyright Copyright (c) 2012, University of Houston all rights reserved.
@license Lesser General Public License Vers. 3, June 2007.
*/

:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_wrapper)).

:- thread_local html_form:'$$form_validate'/1.

:- meta_predicate   validated_form(0, 0).

:- dynamic
  length_input_minmax/5,
  numeric_minmax/5,
  value/4.

:- html_meta error_message(+,html,?,?).

:- meta_predicate form_field(+, 3, +, ?, ?).



%  Handler for a form to be validated.
%%  validated_form(:FormReplyGoal, :LandingReplyGoal) is det
%
%  @param FormReplyGoal is a handler that creates the form page
%  @param LandingReplyGoal is the handler for the landing page.
%
validated_form(FormReplyGoal, LandingReplyGoal) :-
	debug(html_form, '=========~nFormReplyGoal:  ~w~n~nLandingReplyGoal:  ~w',
	      [FormReplyGoal, LandingReplyGoal]),
	setup_call_cleanup(
	    setup_for_form,
      % call twice, because the first one asserts the err message names
      % and the second actually generates the output (like 2 pass compiler)
	    (	with_output_to(string(_), call(FormReplyGoal)),
		with_output_to(string(S), call(FormReplyGoal)),
	        (   has_invalid_entries  ->
	            write(S)  ;
	            call(LandingReplyGoal)
	    )),
	    retractall(html_form:'$$form_validate'(_))
	).

setup_for_form :-
	    retractall(html_form:'$$form_validate'(_)),
	    http_current_request(Request),
	    % doing this because the POST handling in swipl is awkward,
	    % only allows you to call it once
	    http_parameters(Request, [], [form_data(FormData)]),
	    assert(html_form:'$$form_validate'(formdata(FormData))).

%
%%	has_invalid_entries is semidet
%
%       unifies if there are current invalid entries
%
has_invalid_entries  :-
	html_form:'$$form_validate'(validity(_, false)),
	debug(html_form, 'form has invalid entries' , []),
	!.

%  error_message(+Options:list, :Html)// semidet
% DCG to include an error message that will only expand beyond
% nothing if the entry is invalid.
%
% @param Options option list, the only option is required, for=Name,
% case where we can find the for option
%
error_message(Options, _TermHtml) -->
	{
	   memberchk(for=ForTerm, Options),
	   html_form:'$$form_validate'(validity(ForTerm, true)),
	   debug(html_form, 'for=~w is valid', [ForTerm])
	},
	[].

error_message(Options, TermHtml) -->
	{
	   memberchk(for=ForTerm, Options),
	   html_form:'$$form_validate'(validity(ForTerm, false)),
	   debug(html_form, 'for=~w is invalid', [ForTerm])
	},
	TermHtml.

error_message(Options, _TermHtml) -->
	{
	   memberchk(for=ForTerm, Options),
	  \+ html_form:'$$form_validate'(validity(ForTerm, _)),
	   debug(html_form, 'for=~w is unknown validity', [ForTerm])
	},
	html(p('No validity check for ' - ForTerm)).

% when for=Name is missing (user was ebil)
error_message(Options, TermHtml) -->
	{
	    \+ memberchk(for=_, Options),
	    debug(html_form,
	      'Missing for= option in error_message(~w, ~w)',
		  [Options, TermHtml])
	},
	html(p('missing for option')).

%% form_invalidate is det
%
%  always unifies, side effect is marking the form as invalid.
form_invalidate :-
	 assert(html_form:'$$form_validate'(validity(_, false))).

%% form_field(+Request:list, :Validator, +FormField)// is semidet
%
% unifies when form validates
%
form_field(Request, Validator, input(Attribs, Content)) -->
	{
	   memberchk(name=Name, Attribs),
	   html_form:'$$form_validate'(formdata(FormData)),
	   memberchk(Name=Value, FormData),
	   %and validate it
	   call(Validator, Name, Value, Request),
	   assert(html_form:'$$form_validate'(validity(Name, true))),
	   filled_in_field(input(Attribs, Content), Value, FilledInField),
	   debug(html_form, 'the form field ~w=~w validates', [Name, Value])
	},
	html(FilledInField).

%
% parm exists but doesn't validate
%
form_field(Request, Validator, input(Attribs, Content)) -->
	{
	   memberchk(name=Name, Attribs),
	   html_form:'$$form_validate'(formdata(FormData)),
	   memberchk(Name=Value, FormData),
	   %and validate it
	   \+ call(Validator, Name, Value, Request),
	   assert(html_form:'$$form_validate'(validity(Name, false))),
	   filled_in_field(input(Attribs, Content), Value, FilledInField),
	   debug(html_form, 'the form field ~w=~w does not validate', [Name, Value])
	},
	html(FilledInField).


%
% Case where parm doesn't exist in request
%  eg if user is visiting form for first time
%
%
form_field(_Request, _Validator, input(Attribs, Content)) -->
	{
	   memberchk(name=Name, Attribs),
	   html_form:'$$form_validate'(formdata(FormData)),
	   \+ memberchk(Name=_Value, FormData),
	 % 'valid' in that we don't want the err message
	   assert(html_form:'$$form_validate'(validity(Name, true))),
	 % but we need to make sure whole form is invalid
	   assert(html_form:'$$form_validate'(
			      validity('$$notreallyaname', false))),
	   debug(html_form, 'the form field ~w=... does not validate', [Name])
	},
	html(input(Attribs, Content)).

form_field(_Request, _Validator, input(Attribs, Content)) -->
	{
	    \+ memberchk(name=_, Attribs),
	    debug(html_form,
		  'The form field input(~w, ~w) is missing name field',
		  [Attribs, Content])
	},
	html([input(Attribs, Content)]).

% TODO  button, select, textarea, isindex, and a catchall

% Fill in termerized content as if user had typed it in
%
filled_in_field(input(Attribs, InsideHTML), Contents,
	       input(NewAttribs, InsideHTML)) :-
	set_value(Attribs, value=Contents, NewAttribs).

set_value(Name=_, Name=Value, Name=Value) :- !.

set_value(KV, Name=Value, [Name=Value|FreshNCleanKV]) :-
	memberchk(Name=_, KV),
	selectchk(Name=_, KV, FreshNCleanKV).

set_value(KV, Name=Value, [Name=Value|KV]) :-
	is_list(KV),
	\+ memberchk(Name=_, KV).

%
% validates if the length of Value is (Operator) of Length,
% so length_input_minmax(3, '>', _, howdy, _) validates
% (because howdy is of length 5, which is > 3
%
length_input_minmax(Length, Operator, _, Value, _) :-
	write_length(Value, Len, [max_length(65536)]),
	Compare =.. [Operator, Len, Length],
	call(Compare).
%
%  validates if Value is convertable to a number
%  and   (Size, Operator, Value) holds considering
%  Operator as an infix value
%
%  numeric_minmax(Size, Operator, Name, Value, Request)

numeric_minmax(Size, Operator, _, Value, _) :-
	atom_number(Value, Number),
	Compare =.. [Operator, Number, Size],
	call(Compare).

%
% validates if Value is whatever we expect it to be
%
value(Value, _Name, Value, _Request).


http_parameters_quietly(Request, DSL) :-
	catch(
	    http_parameters(Request, DSL),
	    _E,
	    fail).
