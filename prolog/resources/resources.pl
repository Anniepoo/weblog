:- module(resources , []).
/** <module> Define resources for html_requires

   @tbd leaflet requires additional css for IE, but swipl html
   generation isn't handling it

*/
:- use_module(library(http/html_head)).
:- use_module(library(settings)).



:-html_resource(css('demo.css'), []).
:-html_resource(jquery_ui_css, [virtual(true),
       requires(['http://code.jquery.com/ui/1.10.1/themes/base/jquery-ui.css'])]).
:-html_resource(jquery_ui, [virtual(true),
   requires(['http://code.jquery.com/ui/1.10.1/jquery-ui.js',
	     jquery_ui_css,
	     jquery])]).
:-html_resource(jquery, [virtual(true),
       requires(['http://code.jquery.com/jquery-1.9.1.js'])]).

:-html_resource(leaflet, [virtual(true),
       requires(['http://cdn.leafletjs.com/leaflet-0.5/leaflet.css',
		 'http://cdn.leafletjs.com/leaflet-0.5/leaflet.js'])]).

:-html_resource('https://www.google.com/jsapi', [mime_type(text/javascript)]).
:-html_resource(google_loader, [virtual(true),
       requires(['https://www.google.com/jsapi'])]).
:-html_resource(clippy, [virtual(true),
			 requires([jquery, css('clippy.css'), js('clippy.min.js')])
			]).
