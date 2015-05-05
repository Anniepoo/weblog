:- module(wl_box, [
		  abox//2,
		  abox//3
		 ]).

/** <module> Weblog box

Support for generating HTML boxes that curround content.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
*/

:- use_module(library(http/html_write)).

:- html_meta
  abox(+,html,?,?),
  abox(+,+,html,?,?).



%! abox(+Title:atom, +Contents:termized_html)// is det .
% @see Wrapper for abox//3 without additional class names.

abox(Title, Contents) -->
  abox('', Title, Contents).

%! abox(+ClassAdditions:atom, +Title:atom, +Contents:termized_html)// is det.
% Create a round corner rect box with a title area
% above and contents below

abox(ClassAdditions, Title, Contents) -->
	html([
	    div([class='abox header ' + ClassAdditions], p(Title)),
	    div([class='abox content ' + ClassAdditions], Contents)
	     ]).
