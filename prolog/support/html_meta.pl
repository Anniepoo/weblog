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
                  % +Argument
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

:- use_module(weblog(formatting/html_pl_term)).

:- meta_predicate(html_call(3,+,?,?)).

:- html_meta(html_between(html,html,?,?)).
:- html_meta(html_between(html,html,html,?,?)).
:- html_meta(html_call(html,?,?)).
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

%! html_call(:Goal, +Argument)// is det.

html_call(Goal, Arg, X, Y):-
  call(Goal, Arg, X, Y).



%! html_catch(:Goal)// .
% Either generates Goal or an HTML representation of an exception thrown
%  by Goal.

html_catch(Goal, X, Y):-
  catch(
    call(Goal, X, Y),
    Exception,
    call(html_pl_term(Exception), X, Y)
  ).
