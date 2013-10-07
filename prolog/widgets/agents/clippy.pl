:- module(clippy, [clippy//1]).


:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/js_write)).
:- use_module(library(quasi_quotations)).

:- meta_predicate clippy(1, ?, ?).

%!	clippy(+Generator:goal)// is nondet
%
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
