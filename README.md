weblog 
======

Web Library for SWI-Prolog
Rev 0.0.5

Licensed under the Lesser General Public License Vers. 3, June 2007

https://www.gnu.org/copyleft/lesser.html

A copy of which should accompany this library

SWI-Prolog ships with an excellent web framework. This library builds on that work
and is not a 'web framework'. It is, instead, a library of predicates that ease doing
common web tasks.

Some of the code is simply snippets that are extracted from SWI-Prolog related stuff,
or other sources, but I've tried to extract everything to the point that it's a reasonable
library.

Currently, users should expect that almost anything can change.

Install
=======

Other than having a normal SWI-Prolog install, the only install step is to 
clone from github or install from the pack, and set keys for the various functionality 

Setting keys:
For the demo, at the moment, this is the two map providers, google and cloudmade.

copy 
weblog/keys/cloudmadekey.pl.example 
to 
weblog/keys/cloudmadekey.pl
and modify cloudmadekey.pl with your key

repeat for google 


Starting
========

The demo may be started by consulting debug.pl  in the demo and querying weblogdemo:weblog_demo.

Libraries
=========

Every subdirectory has a README.md that explains the contents of that directory

demo     Every major module (ideally) has a demo page. Examining the demo code is often
         an excellent way to learn to use a feature.
         
docs    Documentation for weblog.

under /prolog
    
debug_page  Loading these modules causes your server to serve a couple pages with useful debug info

formatting    tools for assisting with tables, boxes, and other page layout

html_form     validated html forms.

info          subdirectories contain tools for displaying various sorts of information.
              This includes tools for displaying specific types of info, handling feeds
              (geohashing data, geocoding), and often a combination of those (e.g. twitter buttons).
             
keys          Some third party providers (feeds) require a user key be provided. This directory holds
              the keys
              
login_prototype   Tools to make logging in easier. We recognize this is a big project, so for
                  the moment it's a prototype (thanks Thanos!). We'll make a user directory 
                  that handles login, registration, security roles, and profiles some day.
                  
nav           Navigation widgets - menus, links, etc. etc.

resources     resource definitions for other modules, definitions of commonly used javascript libs

static        static files needed by other parts of weblog

support       We're trying really hard to have as small a required footprint as possible. 
              These are a few utility bits that have generally been useful (eg html comments handling).
              
Manifesto
=========

The library should remain bits and pieces you can use as you like, with little cost of inheriting weblog. If all you want is the accordion widget you shouldn't have to change your whole way of coding
to get it.

The only real 'common' pieces are */resources*, which defines the resource inclusion names, and */static*
which holds the various bits of javascript needed by the various widgets. For the moment a truly lean install of
weblog might require a bit of picking through those.

A Common Pattern
================

Web widgets often need a lot of rather trivial parameters and for which there
are often good defaults.

A very common pattern for our widgets is to provide a closure which is called
with a partially instantiated extra argument. The closure instantiates some variables
to return info to the widget. 
This pattern gives the user flexibility. Simple uses can just use facts as the closure.
More complex uses can be described with rules, in 'family tree' style code.

Contributing
============

We emphatically welcome contributions. Because this is more a 'toolbox' library than a unified 
framework, it's particularly easy to contribute.
Weblog is an ambitious project, far beyond what one person can do. It's definitely intended to
be a kitchen sink project. It's Genesis was collecting existing code written for various projects.

Code should generally follow the style conventions of 

http://www.ai.uga.edu/mc/plcoding.pdf

with the 'comma spacing by use' convention.

Comments should generate reasonable pldoc. For a tutorial on pldoc use, see

http://www.pathwayslms.com/swipltuts/pldoc/

Contributions should include a demo page.

So far I've done a laughably bad job of following this myself.


Contributors
============

We'd like to thank the following folks:

 * Anne Ogborn - tables, forms, accordion, maps

 * Thanos Tintinidis   - login and forms

 * Jan Wielemaker - original google maps code, and of course SWI-Prolog!

 * University of Houston - contributed the original library

 * Wouter Beek - converted to pack








