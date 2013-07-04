:- module('html_form/radio', [
			    image_radio_set//1
			   ]).
/**  <module>  Tools for radio buttons

    radio

     Weblog
     Licensed under LGPL
*/

:- use_module(library(http/html_write)).

:- meta_predicate image_radio_set(1, ?, ?).

/**    image_radio_set(+Generator:goal)// is det

      Creates a set of radio buttons with images instead of the default
      rendering

      Generator will be called with one additional argument, of form

      * set_name(Name)     name of the set of radio buttons (optional)

      * id(ID)      one per radio button

      * image(ID, Image)   path to unselected image file for this button

      * selected_image(ID, Image) path to selected image file for this
      button

      * default(ID)  the button to be selected at start
*/
image_radio_set(Generator) -->
	{
	    (	call(Generator, set_name(SetName)) ; gensym(radioset, SetName)),
	    (	bagof(ID, call(Generator, id(ID)), IDList) ; IDList = [] )
	},
	html([
	    \html_post(head,
script(type('text/javascript'), [
                               \['function ~wReset() {~n'-[SetName]],
			       \reset_button_code(Generator, IDList),
                               \['}\n']
				])
	     )]),
	image_radio_buttons(SetName, Generator, IDList).

reset_button_code(_, []) --> [].
reset_button_code(Generator, [H|T]) -->
	{
	   call(Generator, image(H, Image))
	},
	html([
	    \['document.getElementById(\'~w\').checked =false;~n
document.getElementById(\'~wimage\').src = \'~w\';~n'-[H, H, Image]]
	     ]),
	reset_button_code(Generator, T).

image_radio_buttons(_, _, []) --> [].
image_radio_buttons(SetName, Generator, [H|T]) -->
	{
	    call(Generator, image(H, Image)),
	    call(Generator, selected_image(H, SelectedImage)),
	    !,
	    format(atom(Click),
               '~wReset(); document.getElementById(\'~wimage\').src = \'~w\';
document.getElementById(\'~w\').checked =true; ',
               [SetName, H, SelectedImage, H]),
	    (
	        call(Generator, default(H))
	    ->
	        StartImage = SelectedImage,
	        Attribs = [type=radio, id=H, value=H,
			   checked=true, name=SetName]
	    ;
	        StartImage = Image,
	        Attribs = [type=radio, id=H, value=H, name=SetName]
	    )
	},
	html([
	    span(style='display:none', input(Attribs, [])),
	    img([id=H+image,
		 src=StartImage,
		 style='cursor:pointer;',
		 onclick=Click], [])
	     ]),
	image_radio_buttons(SetName, Generator, T).
image_radio_buttons(SetName, Generator, [H|T]) -->
	html([
	    span(['?', '<!-- missing or invalid props for ~w -->'-[H]])
	     ]),
	image_radio_buttons(SetName, Generator, T).


