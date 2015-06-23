:- module(
  bs_hamburger_menu,
  [
    bs_hamburger_menu//2 % +Brand:atom
                         % :Content
  ]
).

/** <module> Bootstrap-based hamburger menu

@author Wouter Beek
@version 2015/06
*/

:- use_module(library(http/html_write)).

:- html_meta(bs_hamburger_menu(+,html,?,?)).





bs_hamburger_menu(Brand, Content) -->
  {Target = target},
  html(
    nav(class=[navbar,'navbar-default'],
      div(class='container-fluid', [
        \bs_hamburger(Brand, Target),
        div([class=[collapse,'navbar-collapse'],id=Target], Content)
      ])
    )
  ).

bs_hamburger(Brand, Target) -->
  {atomic_concat('#', Target, TargetLink)},
  html(
    div(class='navbar-header', [
      button([
        'aria-expanded'=false,
        class=['navbar-toggle',collapsed],
        'data-target'=TargetLink,
        'data-toggle'=collapse,
        type=button
      ], [
        span(class='sr-only', 'Toggle navigation'),
        \bs_bar, \bs_bar, \bs_bar
      ]),
      a([class='navbar-brand',href='/'], Brand)
    ])
  ).

bs_bar -->
  html(span(class='icon-bar', [])).
