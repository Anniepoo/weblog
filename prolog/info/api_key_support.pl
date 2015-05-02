:- module(api_key_support, []).

/** <module> API key support

Provides support for handling API keys.
*/

:- multifile prolog:message//1.

prolog:message(missing_key(Service,File)) -->
  ['Service ~w is missing its key file "~w".'-[Service,File]].
