:- module(html_comments, [ if_ie//2, html_comment//1 ]).
/** <module> Tools for html comments

    Various tools for html comments

    Part of Weblog
    Licensed under the LGPL

*/

:-use_module(library(http/html_write)).
:-html_meta if_ie(+,html,?,?).

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

:- html_meta html_comment(+, ?, ?).

/** html_comment(+Text)// is det

    Insert an html comment
*/
html_comment(Text)-->
	html(\['<!-- ']),
	html_quoted(Text),
	html(\[' -->']).
