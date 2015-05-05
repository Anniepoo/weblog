:- module(
  wl_pl_term,
  [
    wl_pl_term//1, % @Term
    wl_pl_term//2 % +HandleId:atom
                  % @Term
  ]
).

/** <module> Weblog Prolog term

Generates simple HTML representations for Prolog terms.

The HandleId argument allows URIs to be created that can serve
a more complete description of a particular Prolog term.

### Exception format

```prolog
error(
  ErrorType(ErrorSubtype, ErrorTerm),
  context(Predicates, ContextMessage)
)
```

---

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2014-2015
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(wl/format/wl_list)).



%! wl_pl_term(@Term)// is det.

wl_pl_term(Term) -->
  wl_pl_term(_, Term).

%! wl_pl_term(+HandleId:atom, @Term)// is det.
% @see Wrapper around wl_pl_term//2, not using natural language.

% HTML goal.
wl_pl_term(_, goal(Module,Goal)) --> !,
  html(\(Module:Goal)).
% Atom.
wl_pl_term(_, atom(Atom)) --> !,
  html(span(class=atom, Atom)).
% Callable
wl_pl_term(HandleId, callable(Module,Callable)) --> !,
  {compound_name_arity(Callable, Functor, Arity)},
  wl_pl_predicate(HandleId, Module:Functor/Arity).
% Error term.
wl_pl_term(HandleId, exception(Exception)) -->
  wl_pl_error(HandleId, Exception).
% File.
wl_pl_term(_, file(File)) --> !,
  html_file(File).
% Floating point value.
wl_pl_term(_, float(Float)) --> !,
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
wl_pl_term(HandleId, how(Module:How)) --> !,
  wl_pl_how(HandleId, Module, How).
% How class.
wl_pl_term(_, how_class(HowClass)) --> !,
  wl_pl_how_class(HowClass).
% Integer.
wl_pl_term(_, integer(Integer)) --> !,
  wl_pl_integer('~:d', Integer).
% Integer with thousands separators.
wl_pl_term(_, thousands_integer(Integer)) --> !,
  wl_pl_integer('~:D', Integer).
% Module.
% @see http://swi-prolog.org/pldoc/man?section=modules
wl_pl_term(HandleId, module(Module)) --> !,
  wl_pl_module(HandleId, Module).
% Module class.
% `oneof([development,library,system,test,user])`
% @see http://swi-prolog.org/pldoc/doc_for?object=module_property/2
wl_pl_term(_, module_class(ModuleClass)) --> !,
  html(span(class='module-class', ModuleClass)).
% Module operator list.
wl_pl_term(HandleId, operators(Module,Operators)) --> !,
  wl_list(
    Operators,
    [item_writer(wl_pl_operator(HandleId,Module)),ordered(false)]
  ).
% Module predicate list.
wl_pl_term(HandleId, predicates(Module,Predicates)) --> !,
  wl_list(
    Predicates,
    [item_writer(wl_pl_predicate(HandleId,Module)),ordered(false)]
  ).
% Predicate term.
wl_pl_term(HandleId, predicate(Predicate)) --> !,
  wl_pl_predicate(HandleId, Predicate).
% String.
wl_pl_term(_, string(String)) --> !,
  html(span(cass=string, String)).
% Variable.
wl_pl_term(_, var(_)) --> !,
  html(var).
% Hyperlink.
wl_pl_term(_, uri(Uri,Label)) --> !,
  html(a([href=Uri,target=tab], Label)).
% Compound terms are converted to an atom first.
% @tbd Remaining cases: streams, empty list, etc.
wl_pl_term(HandleId, Term) -->
  (   {atom(Term)}
  ->  wl_pl_term(HandleId, atom(Term))
  ;   {float(Term)}
  ->  wl_pl_term(HandleId, float(Term))
  ;   {integer(Term)}
  ->  wl_pl_term(HandleId, integer(Term))
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


%! wl_pl_arity(+Arity:nonneg)// is det.
% Generates an HTML desciption of the artity of a predicate.

wl_pl_arity(Arity) -->
  html(span(class=arity, Arity)).


wl_pl_error(HandleId, error(Formal,Context)) -->
  {Formal =.. [ErrorKind|_]},
  html(
    div(class=error, [
      div(class=error_kind, ErrorKind),
      div(class=error_formal, \wl_pl_error_formal(Formal)),
      \wl_pl_error_context(HandleId, Context)
    ])
  ).


wl_pl_error_action(Action) -->
  html(span(class=action, Action)).


wl_pl_error_context(_, VAR) -->
  {var(VAR)}, !,
  html([]).
wl_pl_error_context(HandleId, context(Predicates,Msg)) -->
  {is_list(Predicates)}, !,
  html(
    div(class=context, [
      \wl_pl_nested_predicate_sequence(HandleId, Predicates),
      \wl_pl_error_message(Msg)
    ])
  ).
wl_pl_error_context(HandleId, context(Predicate,Msg)) -->
  html(
    div(class=context, [
      \wl_pl_predicate(HandleId, Predicate),
      \wl_pl_error_message(Msg)
    ])
  ).
wl_pl_error_context(_, Context) -->
  {atom(Context)}, !,
  html(span(class=context, Context)).


wl_pl_error_formal(VAR) -->
  {var(VAR)}, !, html([]).
% Domain error.
wl_pl_error_formal(domain_error(Type,Term)) -->
  html(
    span(class=domain_error, [
      'The term',
      \wl_pl_error_term(Term),
      'is of the proper type (i.e., ',
      \wl_pl_error_type(Type),
      '), but its value is outside of the domain of supported values.'
    ])
  ).
% Existence error.
wl_pl_error_formal(existence_error(Type,Term)) -->
  html(
    span(class=existence_error, [
      'Term ',
      \wl_pl_error_term(Term),
      ' is of the proper type (i.e., ',
      \wl_pl_error_type(Type),
      ') and is of the correct domain, ',
      'but there is no existing (external) resource represented by it.'
    ])
  ).
% IO error.
wl_pl_error_formal(io_error(Mode,Stream)) -->
  html(
    span(class=io_error, [
      \wl_pl_error_mode(Mode),
      \wl_pl_error_stream(Stream)
    ])
  ).
% Instantiation error.
wl_pl_error_formal(instantiation_error) -->
  html(
    span(class=instantiation_error, [
      'Some terms are under-instantiated.',
      ' I.e. they are not acceptable as is,',
      ' but if some variables were bound to appropriate values ',
      ' it would be acceptable.'
    ])
  ).
wl_pl_error_formal(instantiation_error(Term)) -->
  html(
    span(class=instantiation_error, [
      'Term ',
      \wl_pl_error_term(Term),
      ' is under-instantiated. I.e. it  is not acceptable as is,',
      ' but if some variables were bound to appropriate values',
      ' it would be acceptable.'
    ])
  ).
% Limit exceeded.
wl_pl_error_formal(limit_exceeded(max_errors,Max)) -->
  html(
    span(class=limit_exceeded, [
      'Limit exceeded. Maximum number of errors (i.e., ',
      span(class=max_errors, Max),
     ') reached.'
    ])
  ).
% MIME error.
wl_pl_error_formal(mime_error(_,MustBe,Is)) -->
  html(
    span(class=mime_error, [
      'Must be ',
      span(class=mime, MustBe),
      ' not ',
      span(class=mime,Is)
    ])
  ).
% Permission error.
wl_pl_error_formal(permission_error(Action,Type,Term)) -->
  html(
    span(class=permission_error, [
      \wl_pl_error_action(Action),
      \wl_pl_error_type(Type),
      \wl_pl_error_term(Term)
    ])
  ).
wl_pl_error_formal(permission_error(Action,Type,Term)) -->
  html(
    span(class=permission_error, [
      'It is not allowed to perform action ',
      \wl_pl_error_action(Action),
      ' on the object ',
      \wl_pl_error_term(Term),
      ' that is of type ',
      \wl_pl_error_type(Type),
      '.'
    ])
  ).
% Process error.
wl_pl_error_formal(process_error(Program,exit(Status))) -->
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
wl_pl_error_formal(representation_error(Reason)) -->
  html(
    span(class=representation_error, [
      'Representation error: ',
      span(class=error_reason, Reason)
    ])
  ).
wl_pl_error_formal(representation_error(Reason)) -->
  html(
    span(class=representation_error, [
      'A limitation of the current Prolog implementation is breached: ',
      span(class=error_reason, Reason)
    ])
  ).
% Resource error.
wl_pl_error_formal(resource_error(Reason)) -->
  html(
    span(class=resource_error, [
      'Resource error: ',
      span(class=error_reason, Reason)
    ])
  ).
% Shell error.
wl_pl_error_formal(shell_error(Culprit)) -->
  html(
    span(class=shell_error, [
      'The shell encountered the following error: ',
      code(Culprit)
    ])
  ).
% Socket error.
wl_pl_error_formal(socket_error(Reason)) -->
  html(
    span(class=socket_error, [
      'Socket error: ',
      span(class=error_reason, Reason)
    ])
  ).
% Syntax error.
wl_pl_error_formal(syntax_error(Culprit)) -->
  html(
    span(class=syntax_error, [
      'The following contains invalid syntax: ',
      code(Culprit),
      '.'
    ])
  ).
% Timeout error.
wl_pl_error_formal(timeout_error(Mode,Stream)) -->
  html(
    span(class=timeout_error, [
      'Timeout error: ',
      \wl_pl_error_mode(Mode),
      \wl_pl_error_stream(Stream)
    ])
  ).
% Type error.
wl_pl_error_formal(type_error(Type,Term)) -->
  html(
    span(class=type_error, [
      'Term ',
      \wl_pl_error_term(Term),
      ' is not of type ',
      \wl_pl_error_type(Type),
      '.'
    ])
  ).


wl_pl_error_message(VAR) -->
  {var(VAR)}, !,
  html([]).
wl_pl_error_message(Msg) -->
  html(span(class=message, Msg)).


wl_pl_error_mode(Mode) -->
  html(span(class=mode, Mode)).


wl_pl_error_status_code(StatusCode) -->
  html(span(class=status_code, StatusCode)).


wl_pl_error_stream(Stream) -->
  {with_output_to(atom(Atom), write_canonical(Stream))},
  html(span(class=stream, Atom)).


wl_pl_error_term(Term) -->
  {with_output_to(atom(Atom), write_canonical(Term))},
  html(span(class=term, Atom)).


wl_pl_error_type(Type) -->
  html(span(class=error_type, Type)).


wl_pl_functor(Functor) -->
  html(span(class=functor, Functor)).


wl_pl_functor_and_arity(Functor, Arity) -->
  html([
    \wl_pl_functor(Functor),
    '/',
    \wl_pl_arity(Arity)
  ]).


wl_pl_how(HandleId, Module, How) -->
  {How =.. [imported,_]}, !,
  html(
    span(class=how, [
      \wl_pl_how_class(imported),
      \wl_pl_module(HandleId, Module)
    ])
  ).
wl_pl_how(HandleId, Module, How) -->
  {
    How =.. [HowClass,Line],
    http_link_to_id(HandleId, [line(Line),module(Module)], Uri)
  },
  html(
    span(class=how, [
      \wl_pl_how_class(HowClass),
      span(class=line, a(href=Uri, Line))
    ])
  ).


wl_pl_how_class(HowClass) -->
  html(span(class='how-class', HowClass)).


wl_pl_integer(Format, Integer) -->
  {format(atom(FormattedInteger), Format, [Integer])},
  html(span(class=integer, FormattedInteger)).


wl_pl_module(HandleId, Module) -->
  {http_link_to_id(HandleId, [module(Module)], Uri)},
  html(span(class=module, a(href=Uri, Module))).


wl_pl_nested_predicate_sequence(_, []) --> [].
wl_pl_nested_predicate_sequence(HandleId, [X]) --> !,
  wl_pl_predicate(HandleId, X).
wl_pl_nested_predicate_sequence(HandleId, [H|T]) -->
  html([
    \wl_pl_predicate(HandleId, H),
    ' --> ',
    \wl_pl_nested_predicate_sequence(HandleId, T)
  ]).


wl_pl_operator(HandleId, Module, op(Precedence,Type,Name)) -->
  {
    http_link_to_id(
      HandleId,
      [operator(Module:op(Precedence,Type,Name))],
      Uri
    )
  },
  html(
    span(class=operator, [
      \wl_pl_module(HandleId, Module),
      ':',
      a(href=Uri, [
        'op(',
        span(class=operator_precedence, Precedence),
        ',',
        span(class=operator_type, Type),
        ',',
        span(class=operator_name, \wl_pl_term(HandleId, Name)),
        ')'
      ])
    ])
  ).


wl_pl_predicate(HandleId, Module:Functor/Arity) -->
  wl_pl_predicate(HandleId, Module, Functor, Arity).

wl_pl_predicate(HandleId, Module, Functor/Arity) -->
  wl_pl_predicate(HandleId, Module, Functor, Arity).

wl_pl_predicate(HandleId, Module, Functor, Arity) -->
  {http_link_to_id(HandleId, [predicate(Module:Functor/Arity)], Uri)},
  html(
    span(class=predicate, [
      \wl_pl_module(HandleId, Module),
      ':',
      a(href=Uri, \wl_pl_functor_and_arity(Functor, Arity))
    ])
  ).
