:- module(form_demo, []).

/** <module> Form demo

Generates an HTML demo page for forms.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(wl/form/wl_form)).

:- http_handler(root(form) , form_demo, [id(form)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(form, 'validated forms').

form_demo(Request) :-
	validated_form(
    reply_html_page(
      wl_demo,
		  title('Form Page'),
		  \(form_demo:test_form_content(Request))
    ),
	  reply_html_page(
      wl_demo,
		  title('Thanks'),
		  \(form_demo:test_landing_page_content(Request))
    )
  ).

test_form_content(_) -->
  {debug(wl_form, 'in test_form_content', [])},
  html([
    h1('Validated Form'),
    p('This is the usual sort of validated form. You must enter a name of length >3 and an age over 14 for it to be accepted'),
    style(['.oops {    color: #F00; } ']),
    form([action='/testform', method='POST'], [
	    p([
	      label([for=name],'Name:'),
	      \error_message([for=name], html(p([class=oops],
					  'You need to type your name in here'))),
	          \form_field(Request, length_input_minmax(3, '>'),
			 input([name=name, type=textarea], []))]),
	       p([
	          label([for=age], 'Age:'),
	          \error_message([for=age], html(p([class=oops], 'Age under 14 or not a number'))),
	          \form_field(Request, numeric_minmax(14, '>'),
			 input([name=age, type=textarea], []))]),
	       p([
	           input([type=submit, name=submit, value='Wholy smokes'], [])
	       ])])]).

test_landing_page_content(_Request) -->
	html([
	    p('Well, that worked')
	     ]).

