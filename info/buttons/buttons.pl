:- module(buttons, [reddit//1]).
/** <module> Buttons for various social services

   Reddit - Tool to make reddit button

*/

:- use_module(library(http/html_write)).

% http://www.reddit.com/buttons

reddit(Options) -->
	{
	    option(style(tiny), Options, tiny)
	},
	html([
	\['<a href="http://www.reddit.com/submit" onclick="window.\c
location = \'http://www.reddit.com/submit?url=\' + encodeURIComponent\c
(window.location); return false"> <img src="http://www.reddit.com/static\c
/spreddit1.gif" alt="submit to reddit" border="0" /> </a>']]).

reddit(Options) -->
	{
	    option(style(plus), Options)
	},
	html([
	    \['<a href="http://www.reddit.com/submit" onclick="window.location = \'http://www.reddit.com/submit?url=\' + encodeURIComponent(window.location); return false"> <img src="http://www.reddit.com/static/spreddit7.gif" alt="submit to reddit" border="0" /> </a>']]).

reddit(Options) -->
	{
	    option(style(score_only), Options)
	},
	html([
	    \['<script type="text/javascript" src="http://www.reddit.com/buttonlite.js?i=0"></script>']]).

reddit(Options) -->
	{
	    option(style(tiny_score), Options)
	},
	html([
	    \['<script type="text/javascript" src="http://www.reddit.com/buttonlite.js?i=1"></script>']]).

reddit(Options) -->
	{
	    option(style(tiny_vote), Options)
	},
	html([
	    \['<script type="text/javascript" src="http://www.reddit.com/static/button/button1.js"></script>']]).
