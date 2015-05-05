:- module(social_button, [reddit//1,
		    delicious//1]).
/** <module> Buttons for various social services and feeds

   @tbd  email icon with prefilled subject & body
   @tbd	 twitter
   @tbd  facebook
   @tbd  identi.ca
   @tbd  RSS feed

*/

:- use_module(library(http/html_write)).
:- use_module(library(option)).

% http://www.reddit.com/buttons
/**   reddit(+Options:list)// is det

     Generate a reddit button

     The only option is style(Style)
     The available styles are

     * tiny   an image only button

     * plus   + sign and reddit this

     * score_only    show the score

     * tiny_score   tiny image and the score

     * tiny_vote    tiny image and vote up/down buttons

*/
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

reddit(Options) -->
	{
		throw(error(domain_error(list, Options), context(reddit//1,
				   'invalid style')))
	},
	[].


/**     delicious(+Options:list)// is det

      Creates a button to save this page on del.icio.us

      Options

      * site_name(Name)     Name is an atom which is the site name
                            this is pretty much required

      * save_text(SaveText) Defaults to Save this on Delicious.

*/
delicious(Options) -->
	{
	   option(site_name(CoName), Options, 'set site_name'),
	   option(save_text(SaveText), Options, 'Save this on Delicious'),
	   uri_encoded(query_value, CoName, URLCoName),
	   format(atom(OnClick),
 'window.open(\'http://delicious.com/save?v=5&provider=~w&noui&jump=close&url=\'+
              encodeURIComponent(location.href)+
            \'&title=\'+encodeURIComponent(document.title),
	    \'delicious\',
	    \'toolbar=no,width=550,height=550\'); return false;', [URLCoName] )
	},
	html([img([
		  src('/icons/delicious.gif'),
		      height(16),
		      width(16),
		      alt('Delicious')], []),
	      a([href('#'), onclick(OnClick)], [SaveText])
		]).

