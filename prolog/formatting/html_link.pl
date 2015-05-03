:- module(
  html_link,
  [
    html_external_link//1, % +Uri:atom
    html_external_link//2, % +Uri:atom
                           % +Label:atom
    html_favicon//1, % +Uri:atom
    html_http_handler//1, % +HandleId:atom
    html_image_link//1, % +Image:atom
    html_image_link//2, % +Uri:atom
                        % +Image:atom
    html_link//1, % +Uri:atom
    html_link//2 % +Uri:atom
                 % +Label:atom
  ]
).

/** <module> HTML link

Support for generating various links in HTML.

@author Wouter Beek
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

user:file_search_path(icon, weblog(static/icons)).

:- dynamic(http:location/3).
:- multifile(http:location/3).

http:location(icon, root(icon), []).

:- http_handler(icon(.), serve_files_in_directory(icon), [prefix]).



%! html_external_link(+Uri:atom)// is det.
% @see Wrapper around html_external_link//2 using the URI as
%      the display label.

html_external_link(Uri) -->
  html_external_link(Uri, Uri).

%! html_external_link(+Uri:atom)// is det.
% Generates an HTML link to an external resource.
% The fact that the link points to an external resource is indicated by
% a small anchor image.

html_external_link(Uri, Label) -->
  {http_absolute_location(icon('url.gif'), LinkImage, [])},
  html(a(href=Uri, [Label,img(src=LinkImage)])).


%! html_favicon(+Uri:atom)// is det.
% Generates an HTML link to a favicon.
% This icon will show up in a Web browser's tab.

html_favicon(Uri) -->
  html(link([href=Uri,rel=icon,type='image/x-icon'], [])).


%! html_http_handler(+HandleId:atom)// is det.

html_http_handler(HandleId) -->
  {http_link_to_id(HandleId, [], Location)},
  html(a(href=Location, HandleId)).


%! html_image_link(+Image:atom)// is det.
% Generates an HTML image that links to itself.

html_image_link(Image) -->
  html_image_link(Image, Image).


%! html_image_link(+Uri:atom, +Image:atom)// is det.
% Generates an HTML image that links to the given location.

html_image_link(Uri, Image) -->
  html(a(href=Uri, img(src=Image, []))).


%! html_link(+Uri:atom)// is det.
% Generates an HTML link whose label is the link itself.

html_link(Uri) -->
  html_link(Uri, Uri).


%! html_link(+Uri:atom, +Label:atom)// is det.
% Generates an HTML link together with a display label.

html_link(Uri, Label) -->
  html(a([href=Uri,target=tab], Label)).
