:- module(html_form_demo, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(weblog(html_form/html_form)).

:- http_handler(root(testform) , test_form_page_handler, [id(testform)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(testform, 'Validated Form').

test_form_page_handler(Request) :-
	validated_form(
	    reply_html_page(
		[title('Form Page')],
		\(html_form_demo:test_form_content(Request))),
	    reply_html_page(
		[title('Thanks')],
		\(html_form_demo:test_landing_page_content(Request)))).

test_form_content(_Request) -->
	{
	 debug(html_form, 'in test_form_content~n', [])
	},
     html([
      h1('Validated Form'),
      p('This is the usual sort of validated form. You must enter a name of length >3 and an age over 14 for it to be accepted'),
      style(['.oops {    color: #F00; } ']),
      form([action='/testform', method='POST'], [
	       p([
	          label([for=name],'Name:'),
	          \error_message([for=name], p([class=oops],
					  'You need to type your name in here')),
	          \form_field(Request, length_input_minmax(3, '>'),
			 input([name=name, type=textarea], []))]),
	       p([
	          label([for=age], 'Age:'),
	          \error_message([for=age], p([class=oops], 'Age under 14 or not a number')),
	          \form_field(Request, numeric_minmax(14, '>'),
			 input([name=age, type=textarea], []))]),
	       p([
	           input([type=submit, name=submit, value='Wholy smokes'], [])
	       ])])]).

test_landing_page_content(_Request) -->
	html([
	    p('Well, that worked')
	     ]).

