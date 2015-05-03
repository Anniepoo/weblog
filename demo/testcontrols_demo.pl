:- module(testcontrols_demo, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(library(html_form/radio)).

:- http_handler(root(testcontrols) ,
		test_controls_page_handler,
		[id(testcontrols)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(testcontrols, 'Control Test').

test_controls_page_handler(_Request) :-
	reply_html_page(
    weblog_demo,
	  title('Controls'),
    [
      h1('Some Controls'),
      p('radio buttons using images'),
      form([action='/testcontrols'], [
	      \image_radio_set(radio_info),
	      input([type=submit, name=submitt, value=submitt], [])
      ])
    ]
  ).

radio_info(set_name(demoset)).
radio_info(id(reddot)).
radio_info(id(greendot)).
radio_info(id(bluedot)).
radio_info(image(reddot, '/icons/reddot.png')).
radio_info(selected_image(reddot, '/icons/reddotsel.png')).
radio_info(image(greendot, '/icons/greendot.png')).
radio_info(selected_image(greendot, '/icons/greendotsel.png')).
radio_info(image(bluedot, '/icons/bluedot.png')).
radio_info(selected_image(bluedot, '/icons/bluedotsel.png')).
radio_info(default(reddot)).

