:- module(html_data, [inline_html_file//1]).

/**  <module> HTML data

Deal with direct HTML inclusion.

@author Anne Ogborn
@author Jan Wielemaker
@license Lesser General Public License Vers. 3, June 2007.
*/

:- use_module(library(sgml), [load_html_file/2]).



%! inline_html_file(+Specification)//
% Insert everything in the `body` tag of the specified HTML file.
%
% @author Jan Wielemaker

inline_html_file(Alias) -->
	{
	    absolute_file_name(Alias, Page, [access(read)]),
	    load_html_file(Page, DOM),
	    contains_term(element(body, _, Body), DOM),
	    Style = element(style, _, _),
	    findall(Style, sub_term(Style, DOM), Styles),
	    append(Styles, Body, Content)
	},
	html(Content).

