:- module(
  html_meta,
  [
    html_between//2, % :Outer_0
                     % :Middle_0
    html_between//3, % :Begin_0
                     % :Middle_0
                     % :End_0
    html_call//1, % :Goal_0
    html_call//2, % :Goal_1
                  % +Argument1
    html_call//3, % :Goal_2
                  % +Argument1
                  % +Argument2
    html_call//4, % :Goal_3
                  % +Argument1
                  % +Argument2
                  % +Argument3
    html_catch//1, % :Goal_0
    html_default//3, % :Goal_1
                     % :Default_1
                     % +Argument1
    html_if_then//2, % :If_0
                     % :Then_0
    html_if_then_else//3, % :If_0
                          % :Then_0
                          % :Else_0
    html_ignore//1, % :Content_0
    html_nonvar//1, % :Goal_0
    html_nonvar//2 % :Goal_1
                   % +Argument1
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
:- meta_predicate(html_default(1,1,+,?,?)).
:- meta_predicate(html_nonvar(1,+,?,?)).

:- html_meta(html_between(html,html,?,?)).
:- html_meta(html_between(html,html,html,?,?)).
:- html_meta(html_catch(html,?,?)).
:- html_meta(html_if_then(0,html,?,?)).
:- html_meta(html_if_then_else(0,html,html,?,?)).
:- html_meta(html_ignore(html,?,?)).
:- html_meta(html_nonvar(html,?,?)).





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



%! html_default(:Goal_1, :Default_1, +Argument1)// is det.

html_default(Goal_1, Default_1, Arg1) -->
  var(Goal_1), !,
  html_call(Default_1, Arg1).
html_default(_, Default_1, Arg1) -->
  html_call(Default_1, Arg1).



%! html_if_then(:If, :Then)// is det.

html_if_then(If, Then) -->
  html_if_then_else(If, Then, html([])).



%! html_if_then_else(:If, :Then, :Else)// is det.

html_if_then_else(If, Then, Else) -->
  (   {call(If)}
  ->  Then
  ;   Else
  ).



%! html_ignore(:Content)// is det.

html_ignore(Content) -->
  Content, !.
html_ignore(_) -->
  html([]).



%! html_nonvar(:Goal_0)// is det.

html_nonvar(Goal) -->
  html_if_then(var(Goal), Goal).
