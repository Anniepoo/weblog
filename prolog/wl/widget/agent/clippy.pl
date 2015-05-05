:- module(clippy, [clippy//1]).

/** <module> Clippy

Loads a Clippy virtual agent into a Web page.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(quasi_quotations)).
:- use_module(library(wl/resource/jquery)).

:- meta_predicate clippy(1, ?, ?).

:- html_resource(
  clippy,
  [
    requires([jquery,css('clippy.css'),js('clippy.min.js')]),
    virtual(true)
  ]
).



%! clippy(+Generator:goal)// is nondet
%	Makes clippy appear on screen.
%
%	generator is an arity 1 goal queried with
%	an argument of form
%
%	* character(Char)   Character is one of Clippy, Merlin,
%	  Rover, or Links  (default clippy)
%
%       * id(ID) ID is the ID of the agent (default agent1236742)
%
%	this just brings the character up. You have to control them
%	yourself.
%
% @see Using `clippy-js` from https://www.smore.com/clippy-js

clippy(Generator) -->
	{
             (	  call(Generator, character(Char)) ; Char = 'Clippy' ),
% @tbd The identifier cannot really be set.
%	     (	  call(Generator, id(ID)) ; ID = agent1236742),
% @tbd Is this a typecheck? Then: `must_be(Char, oneof([...]))`
	     member(Char , ['Clippy', 'Merlin', 'Rover', 'Links'])
        },
	html([
	    \html_requires(clippy),
	    \js_script({| javascript(Char) ||
var agent;
$(function() {
    clippy.load(Char, function(agent1236742) {
	agent = agent1236742;

        // Do anything with the loaded agent
        agent.show();
    });
});
|}
		       )]).
