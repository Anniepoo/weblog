:- module(resources , []).
/** <module> Define resources for html_requires
*/
:- use_module(library(http/html_head)).
:- use_module(library(settings)).


/*
 googlekey should contain a single directive like
:- setting(key, atom, 'yourgooglekey',
	   'Google map key.  "abcdefg" works for localhost (didn\'t for me -AO)').
*/
:- include(weblog('keys/googlekey.pl')).
:- setting(script, atom, 'http://maps.google.com/maps?file=api&v=2&sensor=false',
	   'Address of Google map script').

:-html_resource(css('demo.css'), []).
:-html_resource(jquery_ui_css, [virtual(true),
       requires(['http://code.jquery.com/ui/1.10.1/themes/base/jquery-ui.css'])]).
:-html_resource(jquery_ui, [virtual(true),
   requires(['http://code.jquery.com/ui/1.10.1/jquery-ui.js',
	     jquery_ui_css,
	     jquery])]).
:-html_resource(jquery, [virtual(true),
       requires(['http://code.jquery.com/jquery-1.9.1.js'])]).
