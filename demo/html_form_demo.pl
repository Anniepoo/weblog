:- module(html_form_demo, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(weblog(html_form/html_form)).

:- http_handler(root(testform) , test_form_page_handler, [id(testform)]).

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
      form([action='/testform', method='POST'], [
	     label([for=name],'Name:'),
	     \error_message([for=name], p([class=oops],
					  'you have to type a name')),
	     \form_field(Request, length_input_minmax(3, '>'),
			 input([name=name, type=textarea], [])),
	     label([for=age], 'Age:'),
	     \error_message([for=age], p([class=oops], 'Problem with age')),
	     \form_field(Request, numeric_minmax(14, '>'),
			 input([name=age, type=textarea], [])),
	     input([type=submit, name=submit, value='Wholy smokes'], [])])]).

test_landing_page_content(_Request) -->
	html([
	    p('Well, that worked')
	     ]).
