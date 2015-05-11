:- module(radio_demo, []).

/** <module> Radio demo

Generates an HTML demo page for **WebLog** radio options.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_write)).
:- use_module(library(wl/form/wl_radio)).

:- http_handler(root(radio), radio_demo, [id(radio)]).

:- multifile(weblogdemo:label/2).
weblogdemo:label(radio, 'radio options').

radio_demo(_) :-
  http_link_to_id(radio, [], Uri),
	reply_html_page(
    wl_demo,
	  title('Controls'),
    [
      h1('Some Controls'),
      p('radio buttons using images'),
      form(action=Uri, [
	      \image_radio_set(radio_info),
	      input([type=submit, name=submitt, value=submitt], [])
      ])
    ]
  ).

radio_info(set_name(demoset)).
radio_info(id(reddot)).
radio_info(id(greendot)).
radio_info(id(bluedot)).
radio_info(image(reddot,Image)):-
  http_absolute_location(icon('reddot.png'), Image, []).
radio_info(selected_image(reddot, Image)):-
  http_absolute_location(icon('reddotsel.png'), Image, []).
radio_info(image(greendot, Image)):-
  http_absolute_location(icon('greendot.png'), Image, []).
radio_info(selected_image(greendot, Image)):-
  http_absolute_location(icon('greendotsel.png'), Image, []).
radio_info(image(bluedot, Image)):-
  http_absolute_location(icon('bluedot.png'), Image, []).
radio_info(selected_image(bluedot, Image)):-
  http_absolute_location(icon('bluedotsel.png'), Image, []).
radio_info(default(reddot)).

