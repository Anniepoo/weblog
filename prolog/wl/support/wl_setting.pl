:- module(
  wl_setting,
  [
    api_key/2 % +Service:compound
              % -Key:atom
  ]
).

/** <module> WebLog settings

Loads an manages settings for WebLog.

@author Wouter Beek
@version 2015
*/

:- use_module(library(settings)).

:- initialization(init_wl_setting).

init_wl_setting:-
  wl_setting_file(File),
  load_settings(File).



%! api_key(+Service:compound, -Key:atom) is det.

api_key(Service, Key):-
  setting(api_key(Service), Key), !.
api_key(Service, _):-
  print_message(informational, missing_api_key(Service)).



% HELPERS %

wl_setting_file(File):-
  absolute_file_name(library('wl/settings.conf'), File, [access(read)]).



% MESSAGES %

:- multifile prolog:message//1.

prolog:message(missing_api_key(Service)) -->
  {wl_setting_file(File)},
  ['Service ~w is missing its settings file "~w".'-[Service,File]].
