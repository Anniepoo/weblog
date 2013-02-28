:- module(accordion, [accordion//2, acc_header//1]).
/** <module>   Accordion widget



*/
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

:- html_meta accordion(+, html, ?, ?).
:- predicate_options(accordion//2, 1, [
	collapsible(boolean),
	inactive(text),
	active(text),
	height(oneof([fill, content])),
	hoverintent(boolean),
	sortable(boolean)
	       ]).

/**	accordion(+Options:list, +HTML:html)// is det

	Emit an accordion widget.

	Works with library(http/html_write)

	Uses the JQuery library

	accordion has a structural similarity to the html ordered list
	tag

       ==
       <OL>
	    <LI>First Thing</LI>
	    <LI>Second Thing</LI>
       </OL>
       ==

       in that an accordion expects a list of =\accordion_section//2=
       inclusions

       Example:

==
       thing_with_accordion -->
	  html([
	     h2('All of Acmes Fine Products'),
             \accordion([sortable(true)], [
	         \accordion_section('Acme Products #133 Portable Hole', [
		      p(img(src='portablehole.png', []),
		         'The Best hole for the money'),
		      p('Not recommended for use by coyotes')
		      ]),
	          ...
	          \accordion_section('Acme Products #17 Nuclear Bomb', [
		      p(img(src='nuke.png', []),
		         '27Kiloton Nuclear Bomb'),
		      p('The best road runner blaster on themarket') ])
	     ])
==

	Options:
	* collapsible(Collapsible)
	By default, accordions always keep one section open. To allow
	the user to close all sections set collapsible(true).

	* inactive(Class)
	If present, the option active(atom) is required. Inactive headers will receive
	the atom as an additional class.

	* active(Class)
	If present, the active(atom) is required. Inactive headers will receive
	the atom as an additional class.

        * height(Style)
	Because the accordion is comprised of block-level elements, by default its width fills the available horizontal space. To fill the vertical space allocated by its container set height(fill). To consume only as much space as is needed for content, set height(content) (the default).

        * hoverintent(Bool)
	Open sections on hover.

        * sortable(Bool)
	Sections may be rearranged by dragging

	* id(Name)
	Name is the ID to use for the outer div. Defaults to 'accordion'. If you have more than one accordion on a page each will need a unique ID.

	* css(Bool)
	If true (default), include =/themes/base/jquery-ui.css= from the jquery CDN. Setting to false give a very bare boned H3 appearance to the headers, but does work. Set to false if you supply your own styling.

@param Options the list of options
@param HTML the termerized HTML contents, usually a list of
=accordian_section//2= sections


*/

accordion(Options, _, _, _) :-
	option(inactive(_), Options),
	\+ option(active(_), Options),
	throw(error(domain_error(list, Options), context(accordion/2, 'inactive option demands active'))).
accordion(Options, _, _, _) :-
	option(active(_), Options),
	\+ option(inactive(_), Options),
	throw(error(domain_error(list, Options), context(accordion/2, 'active option demands inactive'))).

accordion(Options, HTML) -->
	{
	    debug(weblog, 'accordion got ~q: ~q', [Options, HTML] ),
	    option(id(RawID), Options, accordion),
	    % tries to concat with spaces in html generation
	    % so I do it here
	    format(atom(ID), '"#~w"', [RawID]),
	    !
	},
	accordion_gen(ID, HTML).
accordion(Options, _, _, _) :-
	throw(error(domain_error(list, Options),
		    context(accordion/2, 'Illegal option'))).

accordion_gen(ID, HTML) -->
	{
	    valid_accordion_html(HTML)
	 % check the options, if unhappy throw a bad option exception
	 % well, fail back
	},
	html([
	    \html_requires(jquery),
	    script(['\n
  $(function() {\n
    $(', ID,
     ').accordion();\n
  });\n']),
	    div(id=accordion, HTML)
	     ]).
accordion_gen(_, HTML, _, _) :-
	throw(error(domain_error(list, HTML),
		    context(accordion/2, 'Cannot generate HTML. Only \
accordion_section//2 can be direct child of accordion//2'))).

%%	valid_accordion_html(?HTML:list)
%  unifies if HTML is a list of accordion_section escapes
%
valid_accordion_html([]).
valid_accordion_html([\accordion_section(_, _) | T]) :-
	valid_accordion_html(T).
valid_accordion_html([\(_:accordion_section(_, _)) | T]) :-
	valid_accordion_html(T).


