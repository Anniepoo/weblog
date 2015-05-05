:- module(radio_demo, []).

/** <module> Radio demo

Generates an HTML demo page for **Weblog** radio options.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(wl/form/wl_radio)).

:- http_handler(root(radio), radio_demo, [id(radio)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(radio, 'radio options').

radio_demo(_) :-
	reply_html_page(
    wl_demo,
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

