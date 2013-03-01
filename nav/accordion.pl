:- module(accordion, [accordion//2, accordion_section//2]).
/** <module>   Accordion widget



*/
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

:- ensure_loaded(weblog(resources/resources)).

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

       in that an accordion expects a list of \accordion_section//2
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
	      NOTE: If the containing box is resized after initial draw

	      ==

	       $( "#accordion" ).accordion( "refresh" );

	      ==

	      must be called. See http://jqueryui.com/accordion/#fillspace

        * hover(Bool)
	Open sections on hover.
	      NOTE: This is broken in recent versions of jQuery

        * sortable(Bool)
	Sections may be rearranged by dragging

	* id(Name)
	The outer div's html id (default =accordion= ). Accordions sharing a page need unique IDs.

	* css(Bool)
	If true (default), include =/themes/base/jquery-ui.css= from the jquery CDN. Setting to false give a very bare boned H3 appearance to the headers, but does work. Set to false if you supply your own styling.

@param Options the list of options
@param HTML the termerized HTML contents, which must be a list of
accordian_section//2 sections
@tbd  implement sortable and css options. hover is broken.


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
	    option(id(ID), Options, accordion),
	    option(collapsible(Collapsible), Options, false),
	    option(hover(Hover), Options, false),
	    option(height(Height), Options, content),
	    (	option(active(Active), Options) ->
	        option(inactive(Inactive), Options),
		format(atom(IconText),
'  icons:  {
        header: "~w",
        activeHeader: "~w"
	   }\n', [Inactive, Active])
	    ;
		IconText = ''
	    ),
	    !
	},
	accordion_gen([id(ID),
		       collapsible(Collapsible),
		       icons(IconText),
		       height(Height),
		       hover(Hover)], HTML).
accordion(Options, _, _, _) :-
	throw(error(domain_error(list, Options),
		    context(accordion/2, 'Illegal option'))).

:- html_meta accordion_gen(+, html).

/**  accordion_gen(+Opts:list, HTML:html)// is det

     given a list guaranteed to have all the option values
     generate the actual accordion surround html
*/

accordion_gen(Opts, HTML) -->
	{
	    member(id(ID), Opts),
	    member(collapsible(Collapsible), Opts),
	    member(icons(IconText), Opts),
	    member(hover(Hover), Opts),
	    valid_accordion_html(HTML),
	    (  Collapsible = true ->
	       CollapsePhrase = 'collapsible: true' ;
	       CollapsePhrase = ''
	    ),
	    (	 Hover = true ->
	         HoverPhrase = 'event: "click hoverintent"' ;
	         HoverPhrase = ''
	    ),
	    format(atom(Func),
'$(function() {
   $("#~w").accordion({
~w
~w
~w
		      });
});', [ID, CollapsePhrase, IconText, HoverPhrase]),
	 (   Hover = true ->
	     Addl ='\n\c
 var cfg = ($.hoverintent = {\n\c
    sensitivity: 7,\n\c
    interval: 100\n\c
  });\n\c
 \n\c
  $.event.special.hoverintent = {\n\c
    setup: function() {\n\c
      $( this ).bind( "mouseover", jQuery.event.special.hoverintent.handler );\n\c
    },\n\c
    teardown: function() {\n\c
      $( this ).unbind( "mouseover", jQuery.event.special.hoverintent.handler );\n\c
    },\n\c
    handler: function( event ) {\n\c
      var that = this,\n\c
        args = arguments,\n\c
        target = $( event.target ),\n\c
        cX, cY, pX, pY;\n\c
 \n\c
      function track( event ) {\n\c
        cX = event.pageX;\n\c
        cY = event.pageY;\n\c
      };\n\c
      pX = event.pageX;\n\c
      pY = event.pageY;\n\c
      function clear() {\n\c
        target\n\c
          .unbind( "mousemove", track )\n\c
          .unbind( "mouseout", arguments.callee );\n\c
        clearTimeout( timeout );\n\c
      }\n\c
				\n\c
      function handler() {\n\c
        if ( ( Math.abs( pX - cX ) + Math.abs( pY - cY ) ) < cfg.sensitivity ) {\n\c
          clear();\n\c
          event.type = "hoverintent";\n\c
          // prevent accessing the original event since the new event\n\c
          // is fired asynchronously and the old event is no longer\n\c
          // usable (#6028)\n\c
          event.originalEvent = {};\n\c
          jQuery.event.handle.apply( that, args );\n\c
        } else {\n\c
          pX = cX;\n\c
          pY = cY;\n\c
          timeout = setTimeout( handler, cfg.interval );\n\c
        }\n\c
      }\n\c
      var timeout = setTimeout( handler, cfg.interval );\n\c
      target.mousemove( track ).mouseout( clear );\n\c
      return true;\n\c
    }\n\c
  };'
	 ;
	    Addl = ''
	 ),
	 format(atom(Script), '~w~n~w~n', [Func, Addl])
	},
	html([
	    \html_requires(jquery_ui),
	    script(Script),
	    div(id=ID, HTML)
	     ]).
accordion_gen(_, HTML, _, _) :-
	throw(error(domain_error(list, HTML),
		    context(accordion/2, 'Cannot generate HTML. Only \
accordion_section//2 can be direct child of accordion//2'))).

%%	valid_accordion_html(?HTML:list)
%  unifies if HTML is a list of accordion_section escapes
%
valid_accordion_html(_:X) :-
	valid_accordion_html(X).
valid_accordion_html([]).
valid_accordion_html([\accordion_section(_, _) | T]) :-
	valid_accordion_html(T).
valid_accordion_html([\(_:accordion_section(_, _)) | T]) :-
	valid_accordion_html(T).

:- html_meta accordion_section(+, html, ?, ?).
/**      accordion_section(+Header:options, +HTML:html)// is det

    Create an accordion section of the given header and body.

@param Header atom text of header. In future may accept
option(OptionList)
@param HTML  Termerized HTML for body
@see accordion//2

*/
accordion_section(Header, HTML) -->
	{
	    atomic(Header)
	},
	html([
	    h3(Header),
	    div(HTML)
	     ]).
