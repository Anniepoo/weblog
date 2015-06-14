:- module(
  html_meta,
  [
    html_between//2, % :Outer_2
                     % :Middle_2
    html_between//3, % :Begin_2
                     % :Middle_2
                     % :End_2
    html_call//1, % :Goal_2
    html_call//2, % :Goal_3
                  % +Argument1
    html_call//3, % :Goal_3
                  % +Argument1
                  % +Argument2
    html_call//4, % :Goal_4
                  % +Argument1
                  % +Argument2
                  % +Argument3
    html_catch//1, % :Goal_2
    html_default//3, % :Goal_3
                     % :Default_3
                     % +Argument1
    html_if_then//2, % :If_0
                     % :Then_2
    html_if_then_else//3, % :If_0
                          % :Then_2
                          % :Else_2
    html_ignore//1, % :Content_2
    html_nonvar//1, % :Goal_2
    html_nonvar//2 % :Goal_3
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
:- meta_predicate(html_default(3,3,+,?,?)).
:- meta_predicate(html_nonvar(3,+,?,?)).

:- html_meta(html_between(html,html,?,?)).
:- html_meta(html_between(html,html,html,?,?)).
:- html_meta(html_catch(html,?,?)).
:- html_meta(html_if_then(0,html,?,?)).
:- html_meta(html_if_then_else(0,html,html,?,?)).
:- html_meta(html_ignore(html,?,?)).
:- html_meta(html_nonvar(html,?,?)).





%! html_between(:Outer_2, :Middle_2)// is det.

html_between(Outer_2, Middle_2) -->
  html_between(Outer_2, Middle_2, Outer_2).

%! html_between(:Begin_2, :Middle_2, :End_2)// is det.

html_between(Begin_2, Middle_2, End_2) -->
  html_call(Begin_2),
  html_call(Middle_2),
  html_call(End_2).



%! html_call(:Goal_2)// is det.

html_call(Goal_2, X, Y):-
  call(Goal_2, X, Y).

%! html_call(:Goal_3, +Argument1)// is det.

html_call(Goal_3, Arg1, X, Y):-
  call(Goal_3, Arg1, X, Y).

%! html_call(:Goal_4, +Argument1, +Argument2)// is det.

html_call(Goal_4, Arg1, Arg2, X, Y):-
  call(Goal_4, Arg1, Arg2, X, Y).

%! html_call(:Goal_5, +Argument1, +Argument2, +Argument3)// is det.

html_call(Goal_5, Arg1, Arg2, Arg3, X, Y):-
  call(Goal_5, Arg1, Arg2, Arg3, X, Y).



%! html_catch(:Goal_2)// .
% Either generates Goal or an HTML representation of an exception thrown
%  by Goal.

html_catch(Goal_2, X, Y):-
  catch(
    call(Goal_2, X, Y),
    Error,
    call(wl_pl_term(exception(Error)), X, Y)
  ).



%! html_default(:Goal_3, :Default_3, +Argument1)// is det.

html_default(Goal_3, Default_3, Arg1) -->
  (   {var_goal(Goal_3)}
  ->  html_call(Default_3, Arg1)
  ;   html_call(Goal_3, Arg1)
  ).



%! html_if_then(:If_0, :Then_2)// is det.

html_if_then(If_0, Then_2) -->
  html_if_then_else(If_0, Then_2, html([])).



%! html_if_then_else(:If_0, :Then_2, :Else_2)// is det.

html_if_then_else(If_0, Then_2, Else_2) -->
  (   {call(If_0)}
  ->  html_call(Then_2)
  ;   html_call(Else_2)
  ).



%! html_ignore(:Content_2)// is det.

html_ignore(Content_2) -->
  html_call(Content_2), !.
html_ignore(_) -->
  html([]).



%! html_nonvar(:Goal_2)// is det.

html_nonvar(Goal_2) -->
  (   {var_goal(Goal_2)}
  ->  html([])
  ;   html_call(Goal_2)
  ).

%! html_nonvar(:Goal_3, +Argument1)// is det.

html_nonvar(Goal_3, Arg1) -->
  (   {var_goal(Goal_3)}
  ->  html([])
  ;   html_call(Goal_3, Arg1)
  ).





% HELPERS %

var_goal(X):- var(X), !.
var_goal(_:X):- var(X).
