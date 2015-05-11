:- module(
  html_meta,
  [
    html_between//2, % :Outer
                     % :Middle
    html_between//3, % :Begin
                     % :Middle
                     % :End
    html_call//1, % :Goal
    html_call//2, % :Goal
                  % +Argument1
    html_call//3, % :Goal
                  % +Argument1
                  % +Argument2
    html_call//4, % :Goal
                  % +Argument1
                  % +Argument2
                  % +Argument3
    html_catch//1 % :Goal
  ]
).

/** <module> HTML meta

Catch exceptions that are thrown while generating HTML
and display them in HTML.

---

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/html_write)).
:- use_module(library(wl/format/wl_pl_term)).

:- meta_predicate(html_call(2,?,?)).
:- meta_predicate(html_call(3,+,?,?)).
:- meta_predicate(html_call(4,+,+,?,?)).
:- meta_predicate(html_call(5,+,+,+,?,?)).

:- html_meta(html_between(html,html,?,?)).
:- html_meta(html_between(html,html,html,?,?)).
:- html_meta(html_catch(html,?,?)).



%! html_between(:Outer, :Middle)// is det.

html_between(Outer, Middle) -->
  html_between(Outer, Middle, Outer).

%! html_between(:Begin, :Middle, :End)// is det.

html_between(Begin, Middle, End) -->
  Begin,
  Middle,
  End.



%! html_call(:Goal)// is det.

html_call(Goal, X, Y):-
  call(Goal, X, Y).

%! html_call(:Goal, +Argument1)// is det.

html_call(Goal, Arg1, X, Y):-
  call(Goal, Arg1, X, Y).

%! html_call(:Goal, +Argument1, +Argument2)// is det.

html_call(Goal, Arg1, Arg2, X, Y):-
  call(Goal, Arg1, Arg2, X, Y).

%! html_call(:Goal, +Argument1, +Argument2, +Argument3)// is det.

html_call(Goal, Arg1, Arg2, Arg3, X, Y):-
  call(Goal, Arg1, Arg2, Arg3, X, Y).



%! html_catch(:Goal)// .
% Either generates Goal or an HTML representation of an exception thrown
%  by Goal.

html_catch(Goal, X, Y):-
  catch(
    call(Goal, X, Y),
    Error,
    call(wl_pl_term(exception(Error)), X, Y)
  ).
