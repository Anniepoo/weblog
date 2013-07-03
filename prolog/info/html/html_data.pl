:- module(html_data, [inline_html_file//1]).
/**  <module> Deal with normal style html

*/

:- use_module(library(sgml), [load_html_file/2]).

%%	inline_html_file(+Specification)//
%
%	Insert everything in the =body= tag of
%       a normal html file
%
%       @author	Jan
%

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

