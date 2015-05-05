:- module(
  html_comment,
  [
    if_ie//2,
    html_comment//1 % +Text:atom
  ]
).

/** <module> HTML comment

Support for generating HTML comments.

@author Anne Ogborn
@license Lesser General Public License Vers. 3, June 2007.
@version 2013-2015
*/

:- use_module(library(http/html_write)).

:- html_meta
  html_comment(+, ?, ?),
  if_ie(+,html,?,?).



%! html_comment(+Text:atom)// is det
% Insert an HTML comment.

html_comment(Text)-->
	html(\['<!-- ']),
	html_quoted(Text),
	html(\[' -->']).



/**   if_ie(+Cond, +HTML)// is det

     Conditionally include HTML,
     using the Internet Explorer conditional comments mechanism
     http://www.quirksmode.org/css/condcom.html


     For example

     ==
                   \if('lte IE 8',
                  link([ rel(stylesheet),
                    href('http://cdn.bigco.com/stylesheet.for.ie.css')
                  ]))
    ==

    produces

    ==

    <!--[if lte IE 8]>
      <link rel="stylesheet" href="http://cdn.bigco.com/stylesheet.for.ie.css">
    <![endif]-->

    ==

*/
if_ie(Cond, HTML) -->
    html(\['<!--[if ', Cond, ']>' ]),
    html(HTML),
    html(\['<![endif]-->' ]).
