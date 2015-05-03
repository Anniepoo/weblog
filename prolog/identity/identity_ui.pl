:- module(identity_ui, [
	      login_link//1
	  ]).
/** <module> Widgets for registering, logging in, and so on


*/
:- use_module(library(identity/identity)).


/**	login_link(+Generator:callable)// is det

      Creates a small link that says login or logout and
      links to a separate identity management page

*/
login_link(Generator) -->
	logged_in_as(Generator, Name, _Role),
	{
           call(Generator, show_name),
	   logout_uri(Generator, URI),
	   (   call(Generator, logout_text(Text)) ; Text = ' (logout)')
        },
	html(a(href(URI), Name+Text)).
login_link(Generator) -->
	logged_in_as(Generator, _Name, _Role),
	{
	    logout_uri(Generator, URI),
	   (   call(Generator, logout_text(Text)) ; Text = 'logout')
        },
	html(a(href(URI), Text)).
login_link(Generator) -->
	\+ logged_in_as(Generator, _Name, _Role),
	{
	   login_uri(Generator, URI),
	   (   call(Generator, logout_text(Text)) ; Text = 'Log in')
        },
	html(a(href(URI), Text)).






