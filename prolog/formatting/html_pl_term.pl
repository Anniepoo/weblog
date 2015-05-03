:- module(
  html_pl_term,
  [
    html_pl_term//1, % @Term
    html_pl_term//2 % +HandleId:atom
                    % @Term
  ]
).

/** <module> HTML Prolog term

Generates simple HTML for Prolog terms.

The HandleId argument allows URIs to be created that can serve
a more complete description of a particular Prolog term.

# Exception format

```prolog
error(
  ErrorType(ErrorSubtype, ErrorTerm),
  context(Predicates, ContextMessage)
)
```

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2014-2015
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(library(formatting/html_list)).



%! html_pl_term(@Term)// is det.

html_pl_term(Term) -->
  html_pl_term(_, Term).

%! html_pl_term(+HandleId:atom, @Term)// is det.
% @see Wrapper around html_pl_term//2, not using natural language.

% HTML goal.
html_pl_term(_, goal(Module,Goal)) --> !,
  html(\(Module:Goal)).
% Atom.
html_pl_term(_, atom(Atom)) --> !,
  html(span(class=atom, Atom)).
% Callable
html_pl_term(HandleId, callable(Module,Callable)) --> !,
  {compound_name_arity(Callable, Functor, Arity)},
  html_pl_predicate(HandleId, Module:Functor/Arity).
% Error term.
html_pl_term(HandleId, exception(Exception)) -->
  html_pl_error(HandleId, Exception).
% File.
html_pl_term(_, file(File)) --> !,
  html_file(File).
% Floating point value.
html_pl_term(_, float(Float)) --> !,
  {format(atom(FormattedFloat), '~G', [Float])},
  html(span(class=float, FormattedFloat)).
% How.
% @see http://swi-prolog.org/pldoc/doc_for?object=xref_defined/3
% | `dynamic(Line)`      | Declared dynamic at Line       |
% | `thread_local(Line)` | Declared thread local at Line  |
% | `multifile(Line)`    | Declared multifile at Line     |
% | `local(Line)`        | First clause at Line           |
% | `foreign(Line)`      | Foreign library loaded at Line |
% | `constraint(Line)`   | CHR Constraint at Line         |
% | `imported(File)`     | Imported from File             |
html_pl_term(HandleId, how(Module:How)) --> !,
  html_pl_how(HandleId, Module, How).
% How class.
html_pl_term(_, how_class(HowClass)) --> !,
  html_pl_how_class(HowClass).
% Integer.
html_pl_term(_, integer(Integer)) --> !,
  html_pl_integer('~:d', Integer).
% Integer with thousands separators.
html_pl_term(_, thousands_integer(Integer)) --> !,
  html_pl_integer('~:D', Integer).
% Module.
% @see http://swi-prolog.org/pldoc/man?section=modules
html_pl_term(HandleId, module(Module)) --> !,
  html_pl_module(HandleId, Module).
% Module class.
% `oneof([development,library,system,test,user])`
% @see http://swi-prolog.org/pldoc/doc_for?object=module_property/2
html_pl_term(_, module_class(ModuleClass)) --> !,
  html(span(class='module-class', ModuleClass)).
% Module operator list.
html_pl_term(HandleId, operators(Module,Operators)) --> !,
  html_list(
    Operators,
    [item_writer(html_pl_operator(HandleId,Module)),ordered(false)]
  ).
% Module predicate list.
html_pl_term(HandleId, predicates(Module,Predicates)) --> !,
  html_list(
    Predicates,
    [item_writer(html_pl_predicate(HandleId,Module)),ordered(false)]
  ).
% Predicate term.
html_pl_term(HandleId, predicate(Predicate)) --> !,
  html_pl_predicate(HandleId, Predicate).
% String.
html_pl_term(_, string(String)) --> !,
  html(span(cass=string, String)).
% Variable.
html_pl_term(_, var(_)) --> !,
  html(var).
% Hyperlink.
html_pl_term(_, uri(Uri,Label)) --> !,
  html(a([href=Uri,target=tab], Label)).
% Compound terms are converted to an atom first.
% @tbd Remaining cases: streams, empty list, etc.
html_pl_term(HandleId, Term) -->
  (   {atom(Term)}
  ->  html_pl_term(HandleId, atom(Term))
  ;   {float(Term)}
  ->  html_pl_term(HandleId, float(Term))
  ;   {integer(Term)}
  ->  html_pl_term(HandleId, integer(Term))
  ;   {with_output_to(atom(Atom), write_canonical(Term))},
      html(span(class=compound, Atom))
  ).



% HELPERS %

%! exit_status_reason(+Status:nonneg)// is det.

exit_status_reason(Status) -->
  (   {exit_code_reason(Status, Reason)}
  ->  html(span(class=exit_status_reason, Reason))
  ;   html([])
  ).

exit_code_reason(1, 'Catchall for general/miscellaneous errors.').
exit_code_reason(2, 'Misuse for general errors.').
exit_code_reason(126, 'Command cannot be executed. Permission problem or \c
                       command is not an executable.').
exit_code_reason(127, 'Command not found.').
exit_code_reason(128, 'Invalid argument to the exit command; \c
                       only takes integer args in the range 0-255.').
exit_code_reason(130, 'Script terminated by Control-C.').


%! html_file(+File:atom)// is det.
% Generates an HTML description of the given file name.

html_file(File) -->
  html(span(class=file, File)).


%! html_pl_arity(+Arity:nonneg)// is det.
% Generates an HTML desciption of the artity of a predicate.

html_pl_arity(Arity) -->
  html(span(class=arity, Arity)).


html_pl_error(HandleId, error(Formal,Context)) -->
  {Formal =.. [ErrorKind|_]},
  html(
    div(class=error, [
      div(class=error_kind, ErrorKind),
      div(class=error_formal, \html_pl_error_formal(Formal)),
      \html_pl_error_context(HandleId, Context)
    ])
  ).


html_pl_error_action(Action) -->
  html(span(class=action, Action)).


html_pl_error_context(_, VAR) -->
  {var(VAR)}, !,
  html([]).
html_pl_error_context(HandleId, context(Predicates,Msg)) -->
  {is_list(Predicates)}, !,
  html(
    div(class=context, [
      \html_pl_nested_predicate_sequence(HandleId, Predicates),
      \html_pl_error_message(Msg)
    ])
  ).
html_pl_error_context(HandleId, context(Predicate,Msg)) -->
  html(
    div(class=context, [
      \html_pl_predicate(HandleId, Predicate),
      \html_pl_error_message(Msg)
    ])
  ).
html_pl_error_context(_, Context) -->
  {atom(Context)}, !,
  html(span(class=context, Context)).


html_pl_error_formal(VAR) -->
  {var(VAR)}, !, html([]).
% Domain error.
html_pl_error_formal(domain_error(Type,Term)) -->
  html(
    span(class=domain_error, [
      'The term',
      \html_pl_error_term(Term),
      'is of the proper type (i.e., ',
      \html_pl_error_type(Type),
      '), but its value is outside of the domain of supported values.'
    ])
  ).
% Existence error.
html_pl_error_formal(existence_error(Type,Term)) -->
  html(
    span(class=existence_error, [
      'Term ',
      \html_pl_error_term(Term),
      ' is of the proper type (i.e., ',
      \html_pl_error_type(Type),
      ') and is of the correct domain, ',
      'but there is no existing (external) resource represented by it.'
    ])
  ).
% IO error.
html_pl_error_formal(io_error(Mode,Stream)) -->
  html(
    span(class=io_error, [
      \html_pl_error_mode(Mode),
      \html_pl_error_stream(Stream)
    ])
  ).
% Instantiation error.
html_pl_error_formal(instantiation_error) -->
  html(
    span(class=instantiation_error, [
      'Some terms are under-instantiated.',
      ' I.e. they are not acceptable as is,',
      ' but if some variables were bound to appropriate values ',
      ' it would be acceptable.'
    ])
  ).
html_pl_error_formal(instantiation_error(Term)) -->
  html(
    span(class=instantiation_error, [
      'Term ',
      \html_pl_error_term(Term),
      ' is under-instantiated. I.e. it  is not acceptable as is,',
      ' but if some variables were bound to appropriate values',
      ' it would be acceptable.'
    ])
  ).
% Limit exceeded.
html_pl_error_formal(limit_exceeded(max_errors,Max)) -->
  html(
    span(class=limit_exceeded, [
      'Limit exceeded. Maximum number of errors (i.e., ',
      span(class=max_errors, Max),
     ') reached.'
    ])
  ).
% MIME error.
html_pl_error_formal(mime_error(_,MustBe,Is)) -->
  html(
    span(class=mime_error, [
      'Must be ',
      span(class=mime, MustBe),
      ' not ',
      span(class=mime,Is)
    ])
  ).
% Permission error.
html_pl_error_formal(permission_error(Action,Type,Term)) -->
  html(
    span(class=permission_error, [
      \html_pl_error_action(Action),
      \html_pl_error_type(Type),
      \html_pl_error_term(Term)
    ])
  ).
html_pl_error_formal(permission_error(Action,Type,Term)) -->
  html(
    span(class=permission_error, [
      'It is not allowed to perform action ',
      \html_pl_error_action(Action),
      ' on the object ',
      \html_pl_error_term(Term),
      ' that is of type ',
      \html_pl_error_type(Type),
      '.'
    ])
  ).
% Process error.
html_pl_error_formal(process_error(Program,exit(Status))) -->
  html(
    span(class=process_error, [
     'Process error: ',
      span(class=program, Program),
      ' exited with status ',
      span(class=exit_status, [
        span(class=exit_status_code, Status),
        \exit_status_reason(Status)
      ])
    ])
  ).
% Representation error.
html_pl_error_formal(representation_error(Reason)) -->
  html(
    span(class=representation_error, [
      'Representation error: ',
      span(class=error_reason, Reason)
    ])
  ).
html_pl_error_formal(representation_error(Reason)) -->
  html(
    span(class=representation_error, [
      'A limitation of the current Prolog implementation is breached: ',
      span(class=error_reason, Reason)
    ])
  ).
% Resource error.
html_pl_error_formal(resource_error(Reason)) -->
  html(
    span(class=resource_error, [
      'Resource error: ',
      span(class=error_reason, Reason)
    ])
  ).
% Shell error.
html_pl_error_formal(shell_error(Culprit)) -->
  html(
    span(class=shell_error, [
      'The shell encountered the following error: ',
      code(Culprit)
    ])
  ).
% Socket error.
html_pl_error_formal(socket_error(Reason)) -->
  html(
    span(class=socket_error, [
      'Socket error: ',
      span(class=error_reason, Reason)
    ])
  ).
% Syntax error.
html_pl_error_formal(syntax_error(Culprit)) -->
  html(
    span(class=syntax_error, [
      'The following contains invalid syntax: ',
      code(Culprit),
      '.'
    ])
  ).
% Timeout error.
html_pl_error_formal(timeout_error(Mode,Stream)) -->
  html(
    span(class=timeout_error, [
      'Timeout error: ',
      \html_pl_error_mode(Mode),
      \html_pl_error_stream(Stream)
    ])
  ).
% Type error.
html_pl_error_formal(type_error(Type,Term)) -->
  html(
    span(class=type_error, [
      'Term ',
      \html_pl_error_term(Term),
      ' is not of type ',
      \html_pl_error_type(Type),
      '.'
    ])
  ).


html_pl_error_message(VAR) -->
  {var(VAR)}, !,
  html([]).
html_pl_error_message(Msg) -->
  html(span(class=message, Msg)).


html_pl_error_mode(Mode) -->
  html(span(class=mode, Mode)).


html_pl_error_status_code(StatusCode) -->
  html(span(class=status_code, StatusCode)).


html_pl_error_stream(Stream) -->
  {with_output_to(atom(Atom), write_canonical(Stream))},
  html(span(class=stream, Atom)).


html_pl_error_term(Term) -->
  {with_output_to(atom(Atom), write_canonical(Term))},
  html(span(class=term, Atom)).


html_pl_error_type(Type) -->
  html(span(class=error_type, Type)).


html_pl_functor(Functor) -->
  html(span(class=functor, Functor)).


html_pl_functor_and_arity(Functor, Arity) -->
  html([
    \html_pl_functor(Functor),
    '/',
    \html_pl_arity(Arity)
  ]).


html_pl_how(HandleId, Module, How) -->
  {How =.. [imported,_]}, !,
  html(
    span(class=how, [
      \html_pl_how_class(imported),
      \html_pl_module(HandleId, Module)
    ])
  ).
html_pl_how(HandleId, Module, How) -->
  {
    How =.. [HowClass,Line],
    http_link_to_id(HandleId, [line(Line),module(Module)], Uri)
  },
  html(
    span(class=how, [
      \html_pl_how_class(HowClass),
      span(class=line, a(href=Uri, Line))
    ])
  ).


html_pl_how_class(HowClass) -->
  html(span(class='how-class', HowClass)).


html_pl_integer(Format, Integer) -->
  {format(atom(FormattedInteger), Format, [Integer])},
  html(span(class=integer, FormattedInteger)).


html_pl_module(HandleId, Module) -->
  {http_link_to_id(HandleId, [module(Module)], Uri)},
  html(span(class=module, a(href=Uri, Module))).


html_pl_nested_predicate_sequence(_, []) --> [].
html_pl_nested_predicate_sequence(HandleId, [X]) --> !,
  html_pl_predicate(HandleId, X).
html_pl_nested_predicate_sequence(HandleId, [H|T]) -->
  html([
    \html_pl_predicate(HandleId, H),
    ' --> ',
    \html_pl_nested_predicate_sequence(HandleId, T)
  ]).


html_pl_operator(HandleId, Module, op(Precedence,Type,Name)) -->
  {
    http_link_to_id(
      HandleId,
      [operator(Module:op(Precedence,Type,Name))],
      Uri
    )
  },
  html(
    span(class=operator, [
      \html_pl_module(HandleId, Module),
      ':',
      a(href=Uri, [
        'op(',
        span(class=operator_precedence, Precedence),
        ',',
        span(class=operator_type, Type),
        ',',
        span(class=operator_name, \html_pl_term(HandleId, Name)),
        ')'
      ])
    ])
  ).


html_pl_predicate(HandleId, Module:Functor/Arity) -->
  html_pl_predicate(HandleId, Module, Functor, Arity).

html_pl_predicate(HandleId, Module, Functor/Arity) -->
  html_pl_predicate(HandleId, Module, Functor, Arity).

html_pl_predicate(HandleId, Module, Functor, Arity) -->
  {http_link_to_id(HandleId, [predicate(Module:Functor/Arity)], Uri)},
  html(
    span(class=predicate, [
      \html_pl_module(HandleId, Module),
      ':',
      a(href=Uri, \html_pl_functor_and_arity(Functor, Arity))
    ])
  ).
