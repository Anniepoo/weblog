:- module(autocomplete, [autocomplete//1]).
/** <module> Autocomplete

    jQuery based autocomplete widget

*/
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).

:- ensure_loaded(weblog(resources/resources)).
:- use_module(library(http/js_write)).

:- html_meta autocomplete(1, ?, ?).

/**      autocomplete(+Generator:callable)// is det

Inserts an autocomplete input item

Generator is an arity n term that corresponds to an arity n+1
predicate.

autocomplete//1 will repeatedly query Generator for information and
build up the autocomplete field. The final argument may be

  * choice(-Atom)  A choice for the autocomplete field

  * id(-ID) The input field id will be set to this. default tags

*/
autocomplete(Generator) -->
	{
            (	call(Generator, id(ID))   ;   ID = 'tags' )
        },
	html([
	    \html_requires(jquery_ui),
	    \html_post(head, [\autocomplete_script(Generator)]),
	input([id=ID], [])]).

autocomplete_script(Generator) -->
	{
            call(Generator, accents),!,
            bagof(Choice, call(Generator, choice(Choice)), Choices),
            (	call(Generator, id(ID))   ;   ID = 'tags' )
        },
	html([
	    \js_script( {| javascript(Choices, ID) ||
  $(function() {
    var names = Choices;

    var accentMap = {
      "á": "a",
      "ö": "o"
    };
    var normalize = function( term ) {
      var ret = "";
      for ( var i = 0; i < term.length; i++ ) {
        ret += accentMap[ term.charAt(i) ] || term.charAt(i);
      }
      return ret;
    };

    $( "#"+ID ).autocomplete({
      source: function( request, response ) {
        var matcher = new RegExp( $.ui.autocomplete.escapeRegex( request.term ), "i" );
        response( $.grep( names, function( value ) {
          value = value.label || value.value || value;
          return matcher.test( value ) || matcher.test( normalize( value ) );
        }) );
      }
    });
  });
|} )
	]).

autocomplete_script(Generator) -->
	{
            bagof(Choice, call(Generator, choice(Choice)), Choices),
            (	call(Generator, id(ID))   ;   ID = 'tags' )
        },
	html([
	    \js_script( {| javascript(Choices, ID) ||
$(function() {
    var availableTags = Choices;
    $( "#"+ID ).autocomplete({
      source: availableTags
    });
  });
|} )
	]).
