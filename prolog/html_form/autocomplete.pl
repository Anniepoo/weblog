% Make sure the encoding of the characters `á` and `ö` is
% correctly understood.
:- encoding(utf8).

:- module(autocomplete, [autocomplete//1]).
/** <module> Autocomplete

    jQuery based autocomplete widget

*/
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).

:- ensure_loaded(weblog(resources/resources)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- html_meta autocomplete(1, ?, ?).

/**      autocomplete(+Generator:callable)// is det

Inserts an autocomplete input item

Generator is an arity n term that corresponds to an arity n+1
predicate.

autocomplete//1 will repeatedly query Generator for information and
build up the autocomplete field. The final argument may be

  * choice(-Atom)  A choice for the autocomplete field

  * id(-ID)    The input field id will be set to this. default tags

  * ajax      The generator will be called repeatedly with
  choice(+Term, -Choice) where Term is an atom representation of
  the current contents of the field, and Choice is an atom
  representation of one of the choices


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
            call(Generator, ajax),
            (	call(Generator, id(ID))   ;   ID = 'tags' ),
            ajax_path_name(Generator, PathName)
        },
	html([
	    \js_script( {| javascript(ID, PathName) ||
$(function() {
    $( "#"+ID ).autocomplete({
      source: PathName
    });
  });
|} )
	]).

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

ajax_path_name(Generator, AjaxPath) :-
	(   call(Generator, id(ID)) ; ID = 'tags' ),
	http_current_request(Request),
	member(path(Path), Request),
	atomic_list_concat([Path, '/ajax/', ID], AjaxPath),
	ensure_ajax_handler_exists(Generator, AjaxPath).

ensure_ajax_handler_exists(_, AjaxPath) :-
	http_dispatch:handler(AjaxPath, _, _, _),!.
ensure_ajax_handler_exists(Generator, AjaxPath) :-
	http_handler(AjaxPath, ajax_wrapper(Generator), []).

:- meta_predicate ajax_wrapper(1, +).

ajax_wrapper(Generator, Request) :-
	http_parameters(Request, [
			    term(Term, [])]),
	findall(Choice, call(Generator, choice(Term, Choice)), Choices),
	prolog_to_json(Choices, JSONOut),
        reply_json(JSONOut).

