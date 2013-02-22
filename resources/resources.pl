:- module(resources , []).
/** <module> Define resources for html_requires
*/
:- use_module(library(http/html_head)).

:-html_resource(css('demo.css'), []).
