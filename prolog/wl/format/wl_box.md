> Only problem is, if, in the contents, I include the encapsulation
> escape \,  I run into module problems. Neither  \@(faq_entry,
> indexpage) nor \indexpage:faq_entry are recognized as \term
> encapsulation.
>
> Is there a way to force it to be recognized? I guess I could force
> the caller to call html//1 every time, but that seems an unneccessary
> burden on the caller.


The problem is that \ and : have the wrong priority, and the term
becomes:

1 ?- display(\a:b).
:(\(a),b)
true.

So, you should write

    \(indexpage:faq_entry(...))

>> That said, what is wrong with importing indexpage into the current
>> context?  That also makes the dependency much more explicit.
>
> The only problem with importing it is that you now have a utility
> module that has to explicitly list all it's users.
>
> abox is a bit of code that lays out a widget that might appear
> anywhere in the site. This page
> http://hhpvirtual.net/viewer/splash.html is a mockup for the webapp
> I'm building. The roundrect 'badges' scattered around are what abox
> generates. In design pattern terms, I'm implementing the 'library'
> pattern. I have a library of bits of \foo encapsulation where I'm
> really saying 'I wish HTML had a foo tag'.  So having a list of every
> page on the site at the top of layout.pl (the library file) seems
> wrong.

You must declare your utility predicates as meta-predicates.  There
is a helper directive :- html_meta
http://www.swi-prolog.org/pldoc/doc_for?object=html_write:html_meta/1
that does this for you and provides the cross-referencer and PceEmacs
with the info to understand your code.

> Or have I been programming in Java too much and have a brain
> infection?

I don't know.  It is dangerous though :-)

    Cheers --- Jan
