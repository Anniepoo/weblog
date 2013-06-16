:- module('html_form/radio', [
			    image_radio_set//1
			   ]).
/**  <module>  Tools for radio buttons

    radio

     Weblog
     Licensed under LGPL
*/

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


*/
image_radio_set(Generator) -->
	{
	    (	call(Generator, set_name(SetName)) ; gensym(radioset, SetName)),
	    (	setof(ID, call(Generator, id(ID)), IDList) ; IDList = [] )
	},
	html([
	    \html_post(head,
script(type('text/javascript'), [
                               \['function ~wReset() {~n'-[SetName]],
			       \reset_button_code(IDList),
                               \['}~n']
				])
	     )]),
	image_radio_buttons(SetName, Generator, IDList).

reset_button_code([]) --> [].
reset_button_code([H|T]) -->
	html([
	    \['document.getElementById(\'~w\').checked =false;~n'-[H]]
	     ]),
	reset_button_code(T).

image_radio_buttons(_, _, []) --> [].
image_radio_buttons(SetName, Generator, [H|T]) -->
	{
	    call(Generator, image(H, Image)),
	    call(Generator, selected_image(H, SelectedImage)),
	    !,
	    format(atom(Click),
               '~wReset(); document.getElementById(\'~wimage\').src = \'~w\';
document.getElementById(\'~w\').checked =true; ',
               [SetName, H, SelectedImage, H])
	},
	html([
	    span(style='display:none', input([type=radio, id=H, name=SetName], [])),
	    img([id=H+image,
		 src=Image,
		 style='cursor:pointer;',
		 onclick=Click], [])
	     ]),
	image_radio_buttons(SetName, Generator, T).
image_radio_buttons(SetName, Generator, [H|T]) -->
	html([
	    span(['?', '<!-- missing or invalid props for ~w -->'-[H]])
	     ]),
	image_radio_buttons(SetName, Generator, T).

% TODO need to set default selection
