:- module(
  wl_window,
  [
    wl_popup//3
  ]
).

/** <module> Windows and popups

Support for bringing up windows inside a Web page.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(wl/resource/wl_jquery)).

:- html_meta(wl_popup(1,html,html,?,?)).

:- html_resource(
  js(wl_window),
  [ordered(true),requires([wl_jquery_ui,js('wlwindows.js')]),virtual(true)]
).
:- html_resource(
  css(wl_window),
  [requires([css('wlwindows.css')]),virtual(true)]
).



wl_popup(Options, Activator, Popup) -->
	{findall(Attr, call(Options, Attr), Attrs)},
	html([
    \html_requires(css(wl_window)),
	  \html_requires(js(wl_window)),
	  sup(class=[fn,footnote], [
      Activator,
      span(class=fnp,
        span([class=footcontain|Attrs], Popup)
      )
    ])
	]).
wl_popup(_, _, _) -->
	html(p('missing required options in popup')).
