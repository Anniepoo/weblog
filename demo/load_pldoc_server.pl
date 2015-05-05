:- module(load_pldoc_server, [pldoc/0]).

/** <module> Load pldoc server

Starts a server on localhost that disseminates **weblog**'s documentation.
*/

:- use_module(library(doc_http)).
:- use_module(library(http/http_path)). % HTTP location declarations.
:- use_module(library(www_browser)).

:- multifile http:location/3.

http:location(pldoc, root('help/source'), [priority(10)]).



%! pldoc is det.
%	Run the SWI-Prolog documentation server (pldoc) on port 4040
% of localhost and open the root page.

pldoc:-
	doc_server(4040),
	www_open_url('http://127.0.0.1:4040/help/source').
